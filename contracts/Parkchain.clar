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

(define-data-var next-space-id uint u1)
(define-data-var next-pass-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map parking-spaces
  uint
  {
    owner: principal,
    location: (string-ascii 100),
    price-per-hour: uint,
    is-available: bool,
    total-earnings: uint
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
        total-earnings: u0
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
      (total-cost (* (get price-per-hour space) duration-hours))
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