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
(define-constant ERR_VEHICLE_NOT_FOUND (err u111))
(define-constant ERR_VEHICLE_ALREADY_REGISTERED (err u112))
(define-constant ERR_INVALID_VEHICLE_TYPE (err u113))
(define-constant ERR_VEHICLE_NOT_VERIFIED (err u114))
(define-constant ERR_INCOMPATIBLE_VEHICLE (err u115))
(define-constant ERR_VEHICLE_LIMIT_EXCEEDED (err u116))

(define-data-var next-space-id uint u1)
(define-data-var next-pass-id uint u1)
(define-data-var platform-fee-rate uint u250)
(define-data-var dynamic-pricing-enabled bool true)
(define-data-var surge-multiplier-cap uint u300)
(define-data-var base-demand-threshold uint u5)
(define-data-var max-vehicles-per-user uint u10)
(define-data-var vehicle-verification-fee uint u1000000)
(define-data-var next-vehicle-id uint u1)

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
    peak-hours-end: uint,
    allowed-vehicle-types: (list 10 (string-ascii 20)),
    vehicle-size-limit: uint,
    height-restriction: uint
  }
)

(define-map active-rentals
  uint
  {
    renter: principal,
    space-id: uint,
    start-block: uint,
    end-block: uint,
    total-paid: uint,
    vehicle-id: (optional uint)
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

(define-map registered-vehicles
  uint
  {
    owner: principal,
    license-plate: (string-ascii 20),
    vehicle-type: (string-ascii 20),
    make-model: (string-ascii 50),
    size-category: uint,
    height: uint,
    is-verified: bool,
    verification-block: uint,
    total-parkings: uint,
    registration-block: uint
  }
)

(define-map user-vehicles
  principal
  (list 10 uint)
)

(define-map vehicle-parking-history
  uint
  {
    total-sessions: uint,
    total-duration: uint,
    total-spent: uint,
    last-parking-block: uint,
    violation-count: uint,
    rating-sum: uint,
    rating-count: uint
  }
)

(define-map vehicle-compatibility
  { vehicle-type: (string-ascii 20), space-id: uint }
  {
    is-compatible: bool,
    price-modifier: uint,
    special-requirements: (string-ascii 100)
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
        peak-hours-end: u18,
        allowed-vehicle-types: (list "car" "suv" "truck" "motorcycle"),
        vehicle-size-limit: u300,
        height-restriction: u200
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
  (rent-parking-space-with-vehicle space-id duration-hours none)
)

(define-public (rent-parking-space-with-vehicle (space-id uint) (duration-hours uint) (vehicle-id (optional uint)))
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
    (match vehicle-id
      v-id (try! (validate-vehicle-compatibility v-id space-id))
      true)
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? owner-payment tx-sender (get owner space))))
    (try! (nft-mint? parking-pass pass-id tx-sender))
    (unwrap-panic (update-demand-analytics space-id duration-hours total-cost))
    (match vehicle-id
      v-id (unwrap-panic (update-vehicle-parking-history v-id duration-hours total-cost))
      true)
    (map-set active-rentals pass-id
      {
        renter: tx-sender,
        space-id: space-id,
        start-block: current-block,
        end-block: end-block,
        total-paid: total-cost,
        vehicle-id: vehicle-id
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

(define-public (register-vehicle (license-plate (string-ascii 20)) (vehicle-type (string-ascii 20)) (make-model (string-ascii 50)) (size-category uint) (height uint))
  (let
    (
      (vehicle-id (var-get next-vehicle-id))
      (user-vehicle-list (default-to (list) (map-get? user-vehicles tx-sender)))
      (vehicle-count (len user-vehicle-list))
    )
    (asserts! (< vehicle-count (var-get max-vehicles-per-user)) ERR_VEHICLE_LIMIT_EXCEEDED)
    (asserts! (> (len license-plate) u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (> (len vehicle-type) u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (> size-category u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (> height u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (is-none (get-vehicle-by-license-plate license-plate)) ERR_VEHICLE_ALREADY_REGISTERED)
    (map-set registered-vehicles vehicle-id
      {
        owner: tx-sender,
        license-plate: license-plate,
        vehicle-type: vehicle-type,
        make-model: make-model,
        size-category: size-category,
        height: height,
        is-verified: false,
        verification-block: u0,
        total-parkings: u0,
        registration-block: stacks-block-height
      }
    )
    (map-set user-vehicles tx-sender
      (unwrap-panic (as-max-len? (append user-vehicle-list vehicle-id) u10))
    )
    (map-set vehicle-parking-history vehicle-id
      {
        total-sessions: u0,
        total-duration: u0,
        total-spent: u0,
        last-parking-block: u0,
        violation-count: u0,
        rating-sum: u0,
        rating-count: u0
      }
    )
    (var-set next-vehicle-id (+ vehicle-id u1))
    (ok vehicle-id)
  )
)

(define-public (verify-vehicle (vehicle-id uint))
  (let
    (
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? (var-get vehicle-verification-fee) (get owner vehicle) (as-contract tx-sender)))
    (map-set registered-vehicles vehicle-id
      (merge vehicle {
        is-verified: true,
        verification-block: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (update-vehicle-info (vehicle-id uint) (make-model (string-ascii 50)) (size-category uint) (height uint))
  (let
    (
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner vehicle)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len make-model) u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (> size-category u0) ERR_INVALID_VEHICLE_TYPE)
    (asserts! (> height u0) ERR_INVALID_VEHICLE_TYPE)
    (map-set registered-vehicles vehicle-id
      (merge vehicle {
        make-model: make-model,
        size-category: size-category,
        height: height
      })
    )
    (ok true)
  )
)

(define-public (set-vehicle-compatibility (vehicle-type (string-ascii 20)) (space-id uint) (is-compatible bool) (price-modifier uint) (special-requirements (string-ascii 100)))
  (let
    (
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner space)) ERR_NOT_OWNER)
    (asserts! (>= price-modifier u50) ERR_INVALID_PRICE_MULTIPLIER)
    (asserts! (<= price-modifier u200) ERR_INVALID_PRICE_MULTIPLIER)
    (map-set vehicle-compatibility { vehicle-type: vehicle-type, space-id: space-id }
      {
        is-compatible: is-compatible,
        price-modifier: price-modifier,
        special-requirements: special-requirements
      }
    )
    (ok true)
  )
)

(define-public (rate-vehicle-session (vehicle-id uint) (rating uint))
  (let
    (
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
      (history (default-to
        {
          total-sessions: u0,
          total-duration: u0,
          total-spent: u0,
          last-parking-block: u0,
          violation-count: u0,
          rating-sum: u0,
          rating-count: u0
        }
        (map-get? vehicle-parking-history vehicle-id)
      ))
    )
    (asserts! (>= rating u1) ERR_INVALID_DURATION)
    (asserts! (<= rating u5) ERR_INVALID_DURATION)
    (map-set vehicle-parking-history vehicle-id
      (merge history {
        rating-sum: (+ (get rating-sum history) rating),
        rating-count: (+ (get rating-count history) u1)
      })
    )
    (ok true)
  )
)

(define-public (set-max-vehicles-per-user (new-max uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (>= new-max u1) ERR_INVALID_DURATION)
    (asserts! (<= new-max u20) ERR_INVALID_DURATION)
    (var-set max-vehicles-per-user new-max)
    (ok true)
  )
)

(define-public (set-vehicle-verification-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set vehicle-verification-fee new-fee)
    (ok true)
  )
)

(define-private (validate-vehicle-compatibility (vehicle-id uint) (space-id uint))
  (let
    (
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (vehicle-type (get vehicle-type vehicle))
      (vehicle-size (get size-category vehicle))
      (vehicle-height (get height vehicle))
      (space-size-limit (get vehicle-size-limit space))
      (space-height-limit (get height-restriction space))
      (allowed-types (get allowed-vehicle-types space))
      (compatibility (map-get? vehicle-compatibility { vehicle-type: vehicle-type, space-id: space-id }))
    )
    (asserts! (is-eq tx-sender (get owner vehicle)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-verified vehicle) ERR_VEHICLE_NOT_VERIFIED)
    (asserts! (<= vehicle-size space-size-limit) ERR_INCOMPATIBLE_VEHICLE)
    (asserts! (<= vehicle-height space-height-limit) ERR_INCOMPATIBLE_VEHICLE)
    (asserts! (is-some (index-of allowed-types vehicle-type)) ERR_INCOMPATIBLE_VEHICLE)
    (match compatibility
      comp (asserts! (get is-compatible comp) ERR_INCOMPATIBLE_VEHICLE)
      true)
    (ok true)
  )
)

(define-private (update-vehicle-parking-history (vehicle-id uint) (duration uint) (cost uint))
  (let
    (
      (history (default-to
        {
          total-sessions: u0,
          total-duration: u0,
          total-spent: u0,
          last-parking-block: u0,
          violation-count: u0,
          rating-sum: u0,
          rating-count: u0
        }
        (map-get? vehicle-parking-history vehicle-id)
      ))
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
    )
    (map-set vehicle-parking-history vehicle-id
      (merge history {
        total-sessions: (+ (get total-sessions history) u1),
        total-duration: (+ (get total-duration history) duration),
        total-spent: (+ (get total-spent history) cost),
        last-parking-block: stacks-block-height
      })
    )
    (map-set registered-vehicles vehicle-id
      (merge vehicle {
        total-parkings: (+ (get total-parkings vehicle) u1)
      })
    )
    (ok true)
  )
)

(define-private (get-vehicle-by-license-plate (license-plate (string-ascii 20)))
  (fold check-vehicle-license (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20) none)
)

(define-private (check-vehicle-license (vehicle-id uint) (found (optional uint)))
  (if (is-some found)
    found
    (match (map-get? registered-vehicles vehicle-id)
      vehicle (if (is-eq (get license-plate vehicle) "target-plate") (some vehicle-id) none)
      none
    )
  )
)

(define-read-only (get-vehicle-info (vehicle-id uint))
  (map-get? registered-vehicles vehicle-id)
)

(define-read-only (get-user-vehicles (user principal))
  (default-to (list) (map-get? user-vehicles user))
)

(define-read-only (get-vehicle-parking-history (vehicle-id uint))
  (map-get? vehicle-parking-history vehicle-id)
)

(define-read-only (get-vehicle-compatibility (vehicle-type (string-ascii 20)) (space-id uint))
  (map-get? vehicle-compatibility { vehicle-type: vehicle-type, space-id: space-id })
)

(define-read-only (get-vehicle-rating (vehicle-id uint))
  (match (map-get? vehicle-parking-history vehicle-id)
    history (if (> (get rating-count history) u0)
              (ok (/ (get rating-sum history) (get rating-count history)))
              (ok u0))
    (ok u0)
  )
)

(define-read-only (is-vehicle-verified (vehicle-id uint))
  (match (map-get? registered-vehicles vehicle-id)
    vehicle (get is-verified vehicle)
    false
  )
)

(define-read-only (check-space-vehicle-compatibility (space-id uint) (vehicle-id uint))
  (let
    (
      (vehicle (unwrap! (map-get? registered-vehicles vehicle-id) ERR_VEHICLE_NOT_FOUND))
      (space (unwrap! (map-get? parking-spaces space-id) ERR_SPACE_NOT_FOUND))
      (vehicle-type (get vehicle-type vehicle))
      (vehicle-size (get size-category vehicle))
      (vehicle-height (get height vehicle))
      (space-size-limit (get vehicle-size-limit space))
      (space-height-limit (get height-restriction space))
      (allowed-types (get allowed-vehicle-types space))
      (compatibility (map-get? vehicle-compatibility { vehicle-type: vehicle-type, space-id: space-id }))
      (size-compatible (<= vehicle-size space-size-limit))
      (height-compatible (<= vehicle-height space-height-limit))
      (type-compatible (is-some (index-of allowed-types vehicle-type)))
      (custom-compatible (match compatibility
                           comp (get is-compatible comp)
                           true))
    )
    (ok {
      is-compatible: (and size-compatible height-compatible type-compatible custom-compatible),
      size-compatible: size-compatible,
      height-compatible: height-compatible,
      type-compatible: type-compatible,
      is-verified: (get is-verified vehicle)
    })
  )
)

(define-read-only (get-vehicle-management-settings)
  (ok {
    max-vehicles-per-user: (var-get max-vehicles-per-user),
    verification-fee: (var-get vehicle-verification-fee),
    total-registered-vehicles: (- (var-get next-vehicle-id) u1)
  })
)






