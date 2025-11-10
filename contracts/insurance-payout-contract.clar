(define-constant contract-owner tx-sender)

(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-invalid-policy (err u103))
(define-constant err-policy-expired (err u104))
(define-constant err-already-claimed (err u105))
(define-constant err-claim-not-approved (err u106))

(define-map policies
  { policy-id: uint }
  {
    holder: principal,
    coverage: uint,
    end-block: uint,
    active: bool,
    total-claimed: uint
  }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    amount: uint,
    status: (string-ascii 20)
  }
)

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

(define-public (create-policy (coverage uint) (duration-blocks uint))
  (let (
    (policy-id (var-get next-policy-id))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> coverage u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    (map-set policies
      { policy-id: policy-id }
      {
        holder: tx-sender,
        coverage: coverage,
        end-block: end-block,
        active: true,
        total-claimed: u0
      }
    )
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (submit-claim (policy-id uint) (amount uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (claim-id (var-get next-claim-id))
    (remaining (- (get coverage policy) (get total-claimed policy)))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount remaining) err-invalid-amount)
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        amount: amount,
        status: "pending"
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (approve-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-already-claimed)
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "approved" })
    )
    (ok true)
  )
)

(define-public (process-payout (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found))
    (new-total (+ (get total-claimed policy) (get amount claim)))
    (exhausted (>= new-total (get coverage policy)))
  )
    (asserts! (is-eq (get status claim) "approved") err-claim-not-approved)
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "paid" })
    )
    (map-set policies
      { policy-id: (get policy-id claim) }
      (merge policy {
        total-claimed: new-total,
        active: (not exhausted)
      })
    )
    (ok (get amount claim))
  )
)

(define-public (withdraw-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
  )
    (asserts! (is-eq (get claimant claim) tx-sender) err-owner-only)
    (asserts! (is-eq (get status claim) "pending") err-already-claimed)
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: "withdrawn" })
    )
    (ok true)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (map-set policies
      { policy-id: policy-id }
      (merge policy { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

(define-read-only (get-remaining-coverage (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some (- (get coverage policy) (get total-claimed policy)))
    none
  )
)

(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (and (get active policy) (<= stacks-block-height (get end-block policy)))
    false
  )
)

