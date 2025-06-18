(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-claim-not-approved (err u105))
(define-constant err-policy-expired (err u106))
(define-constant err-already-claimed (err u107))
(define-constant err-invalid-policy (err u108))

(define-map policies
  { policy-id: uint }
  {
    holder: principal,
    premium: uint,
    coverage: uint,
    start-block: uint,
    end-block: uint,
    active: bool
  }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    amount: uint,
    description: (string-ascii 256),
    status: (string-ascii 20),
    submitted-block: uint,
    approved-block: (optional uint)
  }
)

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var contract-balance uint u0)

(define-public (create-policy (coverage uint) (duration-blocks uint))
  (let (
    (policy-id (var-get next-policy-id))
    (premium (/ coverage u10))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> coverage u0) err-invalid-amount)
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      {
        holder: tx-sender,
        premium: premium,
        coverage: coverage,
        start-block: start-block,
        end-block: end-block,
        active: true
      }
    )
    (var-set contract-balance (+ (var-get contract-balance) premium))
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (submit-claim (policy-id uint) (amount uint) (description (string-ascii 256)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (claim-id (var-get next-claim-id))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (<= amount (get coverage policy)) err-invalid-amount)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        amount: amount,
        description: description,
        status: "pending",
        submitted-block: stacks-block-height,
        approved-block: none
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (approve-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-already-claimed)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (>= (var-get contract-balance) (get amount claim)) err-insufficient-funds)
    (map-set claims
      { claim-id: claim-id }
      (merge claim {
        status: "approved",
        approved-block: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (reject-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-already-claimed)
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "rejected" })
    )
    (ok true)
  )
)

(define-public (process-payout (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
    (payout-amount (get amount claim))
  )
    (asserts! (is-eq (get status claim) "approved") err-claim-not-approved)
    (asserts! (>= (var-get contract-balance) payout-amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))))
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "paid" })
    )
    (map-set policies
      { policy-id: (get policy-id claim) }
      (merge policy { active: false })
    )
    (var-set contract-balance (- (var-get contract-balance) payout-amount))
    (ok payout-amount)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (refund-amount (/ (get premium policy) u2))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (>= (var-get contract-balance) refund-amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get holder policy))))
    (map-set policies
      { policy-id: policy-id }
      (merge policy { active: false })
    )
    (var-set contract-balance (- (var-get contract-balance) refund-amount))
    (ok refund-amount)
  )
)

(define-public (add-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok (var-get contract-balance))
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-next-policy-id)
  (var-get next-policy-id)
)

(define-read-only (get-next-claim-id)
  (var-get next-claim-id)
)

(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (and (get active policy) (<= stacks-block-height (get end-block policy)))
    false
  )
)

(define-read-only (get-policy-coverage (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some (get coverage policy))
    none
  )
)

(define-read-only (calculate-premium (coverage uint))
  (/ coverage u10)
)