;; title: Parkchain
;; version: 1.0.0
;; summary: Decentralized parking space rental protocol with NFT-based access control
;; description: A protocol for renting parking spaces using NFTs with time-bound access

;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token parking-pass uint)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_SPACE_NOT_FOUND (err u101))
(define-constant ERR_SPACE_OCCUPIED (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_RENTAL_EXPIRED (err u104))
(define-constant ERR_SPACE_NOT_AVAILABLE (err u105))
(define-constant ERR_INVALID_DURATION (err u106))
(define-constant ERR_NOT_OWNER (err u107))
(define-constant ERR_SPACE_ALREADY_EXISTS (err u108))
(define-constant ERR_INVALID_PRICE_MULTIPLIER (err u109))
(define-constant ERR_PRICING_DISABLED (err u110))

(define-data-var next-space-id uint u1)
(define-data-var next-pass-id uint u1)
(define-data-var platform-fee-rate uint u250)
(define-data-var dynamic-pricing-enabled bool true)
(define-data-var surge-multiplier-cap uint u300)
(define-data-var base-demand-threshold uint u5)

(define-map parking-spaces
  uint
  {
    owner: principal,
    location: (string-ascii 100),
    price-per-hour: uint,
    is-available: bool,
    total-earnings: uint,
    dynamic-pricing-enabled: bool,
    surge-multiplier: uint,
    peak-hours-start: uint,
    peak-hours-end: uint
  }
)

(define-map active-rentals
  uint
  {
    renter: principal,
    space-id: uint,
    start-block: uint,
    end-block: uint,
    total-paid: uint
  }
)

(define-map space-owners
  principal
  (list 50 uint)
)

(define-map user-rentals
  principal
  (list 20 uint)
)

(define-map demand-analytics
  uint
  {
    total-bookings: uint,
    daily-bookings: uint,
    weekly-bookings: uint,
    peak-demand-multiplier: uint,
    average-duration: uint,
    last-booking-block: uint,
    revenue-per-hour: uint
  }
)

(define-map hourly-demand
  { space-id: uint, hour: uint }
  {
    booking-count: uint,
    total-revenue: uint,
    average-price: uint
  }
)

(define-map pricing-history
  { space-id: uint, block-height: uint }
  {
    base-price: uint,
    surge-multiplier: uint,
    final-price: uint,
    demand-level: uint
  }
)

(define-public (create-parking-space (location (string-ascii 100)) (price-per-hour uint))
  (let
    (
      (space-id (var-get next-space-id))
    )
    (asserts! (> price-per-hour u0) ERR_INSUFFICIENT_PAYMENT)
    (map-set parking-spaces space-id
      {
        owner: tx-sender,
        location: location,
        price-per-hour: price-per-hour,
        is-available: true,
        total-earnings: u0,
        dynamic-pricing-enabled: true,
        surge-multiplier: u100,
        peak-hours-start: u8,
        peak-hours-end: u18
      }
    )
    (map-set demand-analytics space-id
      {
        total-bookings: u0,
        daily-bookings: u0,
        weekly-bookings: u0,
        peak-demand-multiplier: u100,
        average-duration: u0,
        last-booking-block: u0,
        revenue-per-hour: u0
      }
    )
    (map-set space-owners tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? space-owners tx-sender)) space-id) u50))
    )
    (var-set next-space-id (+ space-id u1))
    (ok space-id)
  )
)

(define-public (rent-parking-space (space-id uint) (duration-hours uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (pass-id (var-get next-pass-id))
      (blocks-per-hour u144)
      (duration-blocks (* duration-hours blocks-per-hour))
      (dynamic-price (get-dynamic-price space-id duration-hours))
      (total-cost (unwrap-panic dynamic-price))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (owner-payment (- total-cost platform-fee))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
    )
    (asserts! (> duration-hours u0) ERR_INVALID_DURATION)
    (asserts! (<= duration-hours u24) ERR_INVALID_DURATION)
    (asserts! (get is-available space) ERR_SPACE_NOT_AVAILABLE)
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? owner-payment tx-sender (get owner space))))
    (try! (nft-mint? parking-pass pass-id tx-sender))
    (unwrap-panic (update-demand-analytics space-id duration-hours total-cost))
    (map-set active-rentals pass-id
      {
        renter: tx-sender,
        space-id: space-id,
        start-block: current-block,
        end-block: end-block,
        total-paid: total-cost
      }
    )
    (map-set parking-spaces space-id
      (merge space {
        is-available: false,
        total-earnings: (+ (get total-earnings space) owner-payment)
      })
    )
    (map-set user-rentals tx-sender
      (unwrap-panic (as-max-len? (append (default-to (list) (map-get? user-rentals tx-sender)) pass-id) u20))
    )
    (var-set next-pass-id (+ pass-id u1))
    (ok pass-id)
  )
)

(define-public (end-rental (pass-id uint))
  (let
    (
      (rental (unwrap! (map-get? active-rentals pass-id) ERR_SPACE_NOT_FOUND))
      (space-id (get space-id rental))
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get renter rental)) 
                  (is-eq tx-sender (get owner space))
                  (>= stacks-block-height (get end-block rental))) ERR_NOT_AUTHORIZED)
    (map-delete active-rentals pass-id)
    (map-set parking-spaces space-id
      (merge space { is-available: true })
    )
    (try! (nft-burn? parking-pass pass-id (get renter rental)))
    (ok true)
  )
)

(define-public (update-space-price (space-id uint) (new-price uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner space)) ERR_NOT_OWNER)
    (asserts! (get is-available space) ERR_SPACE_OCCUPIED)
    (asserts! (> new-price u0) ERR_INSUFFICIENT_PAYMENT)
    (map-set parking-spaces space-id
      (merge space { price-per-hour: new-price })
    )
    (ok true)
  )
)

(define-public (toggle-space-availability (space-id uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner space)) ERR_NOT_OWNER)
    (map-set parking-spaces space-id
      (merge space { is-available: (not (get is-available space)) })
    )
    (ok (not (get is-available space)))
  )
)

(define-public (extend-rental (pass-id uint) (additional-hours uint))
  (let
    (
      (rental (unwrap! (map-get? active-rentals pass-id) ERR_SPACE_NOT_FOUND))
      (space (unwrap! (map-get? parking-spaces (get space-id rental)) ERR_SPACE_NOT_FOUND))
      (blocks-per-hour u144)
      (additional-blocks (* additional-hours blocks-per-hour))
      (additional-cost (* (get price-per-hour space) additional-hours))
      (platform-fee (/ (* additional-cost (var-get platform-fee-rate)) u10000))
      (owner-payment (- additional-cost platform-fee))
      (new-end-block (+ (get end-block rental) additional-blocks))
    )
    (asserts! (is-eq tx-sender (get renter rental)) ERR_NOT_AUTHORIZED)
    (asserts! (> additional-hours u0) ERR_INVALID_DURATION)
    (asserts! (<= additional-hours u12) ERR_INVALID_DURATION)
    (asserts! (< stacks-block-height (get end-block rental)) ERR_RENTAL_EXPIRED)
    (try! (stx-transfer? additional-cost tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? owner-payment tx-sender (get owner space))))
    (map-set active-rentals pass-id
      (merge rental {
        end-block: new-end-block,
        total-paid: (+ (get total-paid rental) additional-cost)
      })
    )
    (map-set parking-spaces (get space-id rental)
      (merge space {
        total-earnings: (+ (get total-earnings space) owner-payment)
      })
    )
    (ok new-end-block)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR_INVALID_DURATION)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-public (withdraw-platform-fees)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)

(define-read-only (get-parking-space (space-id uint))
  (map-get? parking-spaces space-id)
)

(define-read-only (get-rental-info (pass-id uint))
  (map-get? active-rentals pass-id)
)

(define-read-only (is-rental-active (pass-id uint))
  (match (map-get? active-rentals pass-id)
    rental (< stacks-block-height (get end-block rental))
    false
  )
)

(define-read-only (get-user-spaces (user principal))
  (default-to (list) (map-get? space-owners user))
)

(define-read-only (get-user-rentals (user principal))
  (default-to (list) (map-get? user-rentals user))
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (calculate-rental-cost (space-id uint) (duration-hours uint))
  (match (map-get? parking-spaces space-id)
    space 
      (let
        (
          (total-cost (* (get price-per-hour space) duration-hours))
          (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
        )
        (ok { total-cost: total-cost, platform-fee: platform-fee, owner-payment: (- total-cost platform-fee) })
      )
    ERR_SPACE_NOT_FOUND
  )
)

(define-read-only (get-last-token-id)
  (ok (- (var-get next-pass-id) u1))
)

(define-read-only (get-token-uri (token-id uint))
  (ok none)
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? parking-pass token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
    (nft-transfer? parking-pass token-id sender recipient)
  )
)

(define-public (configure-dynamic-pricing (space-id uint) (enabled bool) (peak-start uint) (peak-end uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner space)) ERR_NOT_OWNER)
    (asserts! (< peak-start u24) ERR_INVALID_DURATION)
    (asserts! (< peak-end u24) ERR_INVALID_DURATION)
    (asserts! (not (is-eq peak-start peak-end)) ERR_INVALID_DURATION)
    (map-set parking-spaces space-id
      (merge space {
        dynamic-pricing-enabled: enabled,
        peak-hours-start: peak-start,
        peak-hours-end: peak-end
      })
    )
    (ok true)
  )
)

(define-public (update-surge-multiplier (space-id uint) (multiplier uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner space)) ERR_NOT_OWNER)
    (asserts! (>= multiplier u50) ERR_INVALID_PRICE_MULTIPLIER)
    (asserts! (<= multiplier (var-get surge-multiplier-cap)) ERR_INVALID_PRICE_MULTIPLIER)
    (map-set parking-spaces space-id
      (merge space { surge-multiplier: multiplier })
    )
    (ok true)
  )
)

(define-public (set-surge-multiplier-cap (new-cap uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (>= new-cap u100) ERR_INVALID_PRICE_MULTIPLIER)
    (asserts! (<= new-cap u500) ERR_INVALID_PRICE_MULTIPLIER)
    (var-set surge-multiplier-cap new-cap)
    (ok true)
  )
)

(define-public (toggle-global-dynamic-pricing)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set dynamic-pricing-enabled (not (var-get dynamic-pricing-enabled)))
    (ok (var-get dynamic-pricing-enabled))
  )
)

(define-private (update-demand-analytics (space-id uint) (duration uint) (revenue uint))
  (let
    (
      (current-analytics (default-to 
        {
          total-bookings: u0,
          daily-bookings: u0,
          weekly-bookings: u0,
          peak-demand-multiplier: u100,
          average-duration: u0,
          last-booking-block: u0,
          revenue-per-hour: u0
        } 
        (map-get? demand-analytics space-id)
      ))
      (new-total-bookings (+ (get total-bookings current-analytics) u1))
      (new-avg-duration (/ (+ (* (get average-duration current-analytics) (get total-bookings current-analytics)) duration) new-total-bookings))
      (new-revenue-per-hour (/ revenue duration))
      (current-hour (mod (/ stacks-block-height u144) u24))
      (current-demand-key { space-id: space-id, hour: current-hour })
      (current-hourly-demand (default-to 
        {
          booking-count: u0,
          total-revenue: u0,
          average-price: u0
        } 
        (map-get? hourly-demand current-demand-key)
      ))
      (new-hourly-bookings (+ (get booking-count current-hourly-demand) u1))
      (new-hourly-revenue (+ (get total-revenue current-hourly-demand) revenue))
      (new-hourly-avg-price (/ new-hourly-revenue new-hourly-bookings))
    )
    (map-set demand-analytics space-id
      (merge current-analytics {
        total-bookings: new-total-bookings,
        daily-bookings: (+ (get daily-bookings current-analytics) u1),
        weekly-bookings: (+ (get weekly-bookings current-analytics) u1),
        average-duration: new-avg-duration,
        last-booking-block: stacks-block-height,
        revenue-per-hour: new-revenue-per-hour
      })
    )
    (map-set hourly-demand current-demand-key
      {
        booking-count: new-hourly-bookings,
        total-revenue: new-hourly-revenue,
        average-price: new-hourly-avg-price
      }
    )
    (ok true)
  )
)

(define-private (get-dynamic-price (space-id uint) (duration uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (base-price (get price-per-hour space))
      (dynamic-enabled (get dynamic-pricing-enabled space))
      (global-dynamic-enabled (var-get dynamic-pricing-enabled))
      (current-hour (mod (/ stacks-block-height u144) u24))
      (peak-start (get peak-hours-start space))
      (peak-end (get peak-hours-end space))
      (is-peak-time (if (< peak-start peak-end)
                      (and (>= current-hour peak-start) (< current-hour peak-end))
                      (or (>= current-hour peak-start) (< current-hour peak-end))))
      (analytics (default-to
        {
          total-bookings: u0,
          daily-bookings: u0,
          weekly-bookings: u0,
          peak-demand-multiplier: u100,
          average-duration: u0,
          last-booking-block: u0,
          revenue-per-hour: u0
        }
        (map-get? demand-analytics space-id)
      ))
      (demand-level (get daily-bookings analytics))
      (surge-multiplier (get surge-multiplier space))
      (demand-multiplier (if (> demand-level (var-get base-demand-threshold))
                           (let ((calculated-multiplier (+ u100 (* (- demand-level (var-get base-demand-threshold)) u10))))
                             (if (< calculated-multiplier (var-get surge-multiplier-cap))
                               calculated-multiplier
                               (var-get surge-multiplier-cap)))
                           u100))
      (peak-multiplier (if is-peak-time u120 u100))
      (final-multiplier (if (and dynamic-enabled global-dynamic-enabled)
                          (/ (* surge-multiplier demand-multiplier peak-multiplier) u10000)
                          u100))
      (adjusted-price (/ (* base-price final-multiplier) u100))
      (total-cost (* adjusted-price duration))
    )
    (map-set pricing-history { space-id: space-id, block-height: stacks-block-height }
      {
        base-price: base-price,
        surge-multiplier: final-multiplier,
        final-price: adjusted-price,
        demand-level: demand-level
      }
    )
    (ok total-cost)
  )
)

(define-read-only (get-current-price (space-id uint) (duration uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (base-price (get price-per-hour space))
      (dynamic-enabled (get dynamic-pricing-enabled space))
      (global-dynamic-enabled (var-get dynamic-pricing-enabled))
      (current-hour (mod (/ stacks-block-height u144) u24))
      (peak-start (get peak-hours-start space))
      (peak-end (get peak-hours-end space))
      (is-peak-time (if (< peak-start peak-end)
                      (and (>= current-hour peak-start) (< current-hour peak-end))
                      (or (>= current-hour peak-start) (< current-hour peak-end))))
      (analytics (default-to
        {
          total-bookings: u0,
          daily-bookings: u0,
          weekly-bookings: u0,
          peak-demand-multiplier: u100,
          average-duration: u0,
          last-booking-block: u0,
          revenue-per-hour: u0
        }
        (map-get? demand-analytics space-id)
      ))
      (demand-level (get daily-bookings analytics))
      (surge-multiplier (get surge-multiplier space))
      (demand-multiplier (if (> demand-level (var-get base-demand-threshold))
                           (let ((calculated-multiplier (+ u100 (* (- demand-level (var-get base-demand-threshold)) u10))))
                             (if (< calculated-multiplier (var-get surge-multiplier-cap))
                               calculated-multiplier
                               (var-get surge-multiplier-cap)))
                           u100))
      (peak-multiplier (if is-peak-time u120 u100))
      (final-multiplier (if (and dynamic-enabled global-dynamic-enabled)
                          (/ (* surge-multiplier demand-multiplier peak-multiplier) u10000)
                          u100))
      (adjusted-price (/ (* base-price final-multiplier) u100))
      (total-cost (* adjusted-price duration))
    )
    (ok total-cost)
  )
)

(define-read-only (get-demand-analytics (space-id uint))
  (map-get? demand-analytics space-id)
)

(define-read-only (get-hourly-demand (space-id uint) (hour uint))
  (map-get? hourly-demand { space-id: space-id, hour: hour })
)

(define-read-only (get-pricing-history (space-id uint) (target-block uint))
  (map-get? pricing-history { space-id: space-id, block-height: target-block })
)

(define-read-only (get-peak-hours (space-id uint))
  (match (map-get? parking-spaces space-id)
    space (ok { 
      peak-start: (get peak-hours-start space), 
      peak-end: (get peak-hours-end space),
      dynamic-enabled: (get dynamic-pricing-enabled space)
    })
    ERR_SPACE_NOT_FOUND
  )
)

(define-read-only (calculate-surge-pricing (space-id uint) (duration uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (base-cost (* (get price-per-hour space) duration))
      (dynamic-cost (unwrap-panic (get-current-price space-id duration)))
      (surge-percentage (if (> dynamic-cost base-cost)
                          (/ (* (- dynamic-cost base-cost) u100) base-cost)
                          u0))
    )
    (ok {
      base-cost: base-cost,
      dynamic-cost: dynamic-cost,
      surge-percentage: surge-percentage,
      savings: (if (< dynamic-cost base-cost) (- base-cost dynamic-cost) u0)
    })
  )
)

(define-read-only (get-demand-forecast (space-id uint))
  (let
    (
      (analytics (default-to
        {
          total-bookings: u0,
          daily-bookings: u0,
          weekly-bookings: u0,
          peak-demand-multiplier: u100,
          average-duration: u0,
          last-booking-block: u0,
          revenue-per-hour: u0
        }
        (map-get? demand-analytics space-id)
      ))
      (daily-bookings (get daily-bookings analytics))
      (weekly-bookings (get weekly-bookings analytics))
      (trend (if (> weekly-bookings (* daily-bookings u7)) "increasing" "stable"))
      (projected-daily (if (> weekly-bookings u0) (/ weekly-bookings u7) daily-bookings))
      (utilization-rate (if (> projected-daily u0) 
                        (let ((calculated-rate (/ (* projected-daily u100) u24)))
                          (if (< calculated-rate u100) calculated-rate u100))
                        u0))
    )
    (ok {
      current-daily-bookings: daily-bookings,
      projected-daily-bookings: projected-daily,
      trend: trend,
      utilization-rate: utilization-rate,
      revenue-per-hour: (get revenue-per-hour analytics)
    })
  )
)

(define-read-only (get-optimal-pricing (space-id uint))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (base-price (get price-per-hour space))
      (analytics (default-to
        {
          total-bookings: u0,
          daily-bookings: u0,
          weekly-bookings: u0,
          peak-demand-multiplier: u100,
          average-duration: u0,
          last-booking-block: u0,
          revenue-per-hour: u0
        }
        (map-get? demand-analytics space-id)
      ))
      (daily-bookings (get daily-bookings analytics))
      (revenue-per-hour (get revenue-per-hour analytics))
      (utilization-rate (if (> daily-bookings u0) 
                        (let ((calculated-rate (/ (* daily-bookings u100) u24)))
                          (if (< calculated-rate u100) calculated-rate u100))
                        u0))
      (optimal-multiplier (if (< utilization-rate u70) u90
                            (if (< utilization-rate u90) u100 u110)))
      (optimal-price (/ (* base-price optimal-multiplier) u100))
    )
    (ok {
      current-price: base-price,
      optimal-price: optimal-price,
      utilization-rate: utilization-rate,
      revenue-per-hour: revenue-per-hour,
      recommended-action: (if (< utilization-rate u70) "decrease-price" 
                            (if (< utilization-rate u90) "maintain-price" "increase-price"))
    })
  )
)

(define-read-only (get-dynamic-pricing-settings)
  (ok {
    global-enabled: (var-get dynamic-pricing-enabled),
    surge-cap: (var-get surge-multiplier-cap),
    base-demand-threshold: (var-get base-demand-threshold)
  })
)