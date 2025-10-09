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
(define-constant err-invalid-age (err u109))
(define-constant err-invalid-duration (err u110))
(define-constant err-policy-not-renewable (err u111))
(define-constant err-renewal-too-early (err u112))
(define-constant err-invalid-beneficiary (err u113))
(define-constant err-unauthorized-beneficiary (err u114))
(define-constant err-invalid-percentage (err u115))
(define-constant err-beneficiaries-full (err u116))
(define-constant err-coverage-exceeded (err u117))
(define-constant err-no-coverage-remaining (err u118))

(define-map policies
  { policy-id: uint }
  {
    holder: principal,
    premium: uint,
    coverage: uint,
    start-block: uint,
    end-block: uint,
    active: bool,
    age: uint,
    risk-score: uint,
    renewable: bool,
    renewal-count: uint,
    total-claimed: uint,
    claim-count: uint
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

(define-map beneficiaries
  { policy-id: uint, beneficiary: principal }
  {
    percentage: uint,
    active: bool,
    added-block: uint
  }
)

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var contract-balance uint u0)

(define-public (create-policy (coverage uint) (duration-blocks uint) (age uint))
  (let (
    (policy-id (var-get next-policy-id))
    (risk-score (calculate-risk-score age coverage duration-blocks))
    (premium (calculate-dynamic-premium coverage risk-score))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> coverage u0) err-invalid-amount)
    (asserts! (and (>= age u18) (<= age u100)) err-invalid-age)
    (asserts! (and (>= duration-blocks u144) (<= duration-blocks u52560)) err-invalid-duration)
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      {
        holder: tx-sender,
        premium: premium,
        coverage: coverage,
        start-block: start-block,
        end-block: end-block,
        active: true,
        age: age,
        risk-score: risk-score,
        renewable: true,
        renewal-count: u0,
        total-claimed: u0,
        claim-count: u0
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
    (remaining-coverage (- (get coverage policy) (get total-claimed policy)))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (> remaining-coverage u0) err-no-coverage-remaining)
    (asserts! (<= amount remaining-coverage) err-coverage-exceeded)
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
    (new-total-claimed (+ (get total-claimed policy) payout-amount))
    (new-claim-count (+ (get claim-count policy) u1))
    (coverage-exhausted (>= new-total-claimed (get coverage policy)))
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
      (merge policy { 
        total-claimed: new-total-claimed,
        claim-count: new-claim-count,
        active: (not coverage-exhausted),
        renewable: (if coverage-exhausted false (get renewable policy))
      })
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
      (merge policy { active: false, renewable: false })
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

(define-read-only (calculate-risk-score (age uint) (coverage uint) (duration-blocks uint))
  (let (
    (age-factor (if (<= age u25) u150
                  (if (<= age u40) u100
                    (if (<= age u60) u125
                      u175))))
    (coverage-factor (if (<= coverage u50000) u100
                      (if (<= coverage u200000) u125
                        u150)))
    (duration-factor (if (<= duration-blocks u4380) u100
                      (if (<= duration-blocks u17520) u110
                        u125)))
  )
    (/ (+ (* age-factor coverage-factor) (* duration-factor u50)) u150)
  )
)

(define-read-only (calculate-dynamic-premium (coverage uint) (risk-score uint))
  (let (
    (base-premium (/ coverage u20))
    (risk-multiplier (+ u100 (/ (* risk-score u50) u100)))
  )
    (/ (* base-premium risk-multiplier) u100)
  )
)

(define-read-only (get-policy-risk-score (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some (get risk-score policy))
    none
  )
)

(define-read-only (estimate-premium (coverage uint) (age uint) (duration-blocks uint))
  (let (
    (risk-score (calculate-risk-score age coverage duration-blocks))
  )
    (calculate-dynamic-premium coverage risk-score)
  )
)

(define-public (renew-policy (policy-id uint) (extension-blocks uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (current-age (+ (get age policy) (/ (- stacks-block-height (get start-block policy)) u52560)))
    (renewal-premium (calculate-renewal-premium policy extension-blocks current-age))
    (new-end-block (+ (get end-block policy) extension-blocks))
    (grace-period u1440)
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get renewable policy) err-policy-not-renewable)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (<= (get renewal-count policy) u5) err-policy-not-renewable)
    (asserts! (>= stacks-block-height (- (get end-block policy) grace-period)) err-renewal-too-early)
    (asserts! (and (>= extension-blocks u144) (<= extension-blocks u52560)) err-invalid-duration)
    (try! (stx-transfer? renewal-premium tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      (merge policy {
        end-block: new-end-block,
        premium: (+ (get premium policy) renewal-premium),
        renewal-count: (+ (get renewal-count policy) u1)
      })
    )
    (var-set contract-balance (+ (var-get contract-balance) renewal-premium))
    (ok new-end-block)
  )
)

(define-read-only (calculate-renewal-premium (policy {holder: principal, premium: uint, coverage: uint, start-block: uint, end-block: uint, active: bool, age: uint, risk-score: uint, renewable: bool, renewal-count: uint, total-claimed: uint, claim-count: uint}) (extension-blocks uint) (current-age uint))
  (let (
    (base-premium (/ (* (get coverage policy) extension-blocks) u525600))
    (loyalty-discount (if (>= (get renewal-count policy) u3) u90 u95))
    (age-adjustment (if (> current-age u65) u110 u100))
  )
    (/ (* (* base-premium loyalty-discount) age-adjustment) u10000)
  )
)

(define-read-only (get-renewal-eligibility (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (let (
      (grace-period u1440)
      (blocks-until-expiry (- (get end-block policy) stacks-block-height))
    )
      (some {
        renewable: (get renewable policy),
        active: (get active policy),
        within-grace-period: (<= blocks-until-expiry grace-period),
        renewals-remaining: (- u5 (get renewal-count policy)),
        blocks-until-expiry: blocks-until-expiry
      })
    )
    none
  )
)

(define-read-only (estimate-renewal-cost (policy-id uint) (extension-blocks uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (let (
      (current-age (+ (get age policy) (/ (- stacks-block-height (get start-block policy)) u52560)))
    )
      (some (calculate-renewal-premium policy extension-blocks current-age))
    )
    none
  )
)

(define-read-only (get-policy-renewal-history (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some {
      renewal-count: (get renewal-count policy),
      total-premium-paid: (get premium policy),
      original-end-block: (+ (get start-block policy) u52560),
      current-end-block: (get end-block policy)
    })
    none
  )
)

(define-public (set-beneficiary (policy-id uint) (beneficiary principal) (percentage uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (current-total (get-beneficiary-total-percentage policy-id))
    (existing-beneficiary (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary }))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (> percentage u0) err-invalid-percentage)
    (asserts! (<= percentage u100) err-invalid-percentage)
    (asserts! (is-none existing-beneficiary) err-already-exists)
    (asserts! (<= (+ current-total percentage) u100) err-invalid-percentage)
    (map-set beneficiaries
      { policy-id: policy-id, beneficiary: beneficiary }
      {
        percentage: percentage,
        active: true,
        added-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (update-beneficiary-percentage (policy-id uint) (beneficiary principal) (new-percentage uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (beneficiary-info (unwrap! (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary }) err-not-found))
    (current-total (get-beneficiary-total-percentage policy-id))
    (adjusted-total (- current-total (get percentage beneficiary-info)))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active beneficiary-info) err-invalid-beneficiary)
    (asserts! (> new-percentage u0) err-invalid-percentage)
    (asserts! (<= new-percentage u100) err-invalid-percentage)
    (asserts! (<= (+ adjusted-total new-percentage) u100) err-invalid-percentage)
    (map-set beneficiaries
      { policy-id: policy-id, beneficiary: beneficiary }
      (merge beneficiary-info { percentage: new-percentage })
    )
    (ok true)
  )
)

(define-public (remove-beneficiary (policy-id uint) (beneficiary principal))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (beneficiary-info (unwrap! (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary }) err-not-found))
  )
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (get active beneficiary-info) err-invalid-beneficiary)
    (map-set beneficiaries
      { policy-id: policy-id, beneficiary: beneficiary }
      (merge beneficiary-info { active: false })
    )
    (ok true)
  )
)

(define-public (beneficiary-claim (policy-id uint) (claim-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
    (beneficiary-info (unwrap! (map-get? beneficiaries { policy-id: policy-id, beneficiary: tx-sender }) err-not-found))
    (payout-amount (get amount claim))
    (beneficiary-share (/ (* payout-amount (get percentage beneficiary-info)) u100))
  )
    (asserts! (is-eq (get policy-id claim) policy-id) err-invalid-policy)
    (asserts! (is-eq (get status claim) "approved") err-claim-not-approved)
    (asserts! (get active beneficiary-info) err-unauthorized-beneficiary)
    (asserts! (get active policy) err-invalid-policy)
    (asserts! (>= (var-get contract-balance) beneficiary-share) err-insufficient-funds)
    (try! (as-contract (stx-transfer? beneficiary-share tx-sender tx-sender)))
    (var-set contract-balance (- (var-get contract-balance) beneficiary-share))
    (ok beneficiary-share)
  )
)

(define-read-only (get-beneficiary-total-percentage (policy-id uint))
  u0
)

(define-read-only (get-beneficiary-info (policy-id uint) (beneficiary principal))
  (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary })
)

(define-read-only (is-authorized-beneficiary (policy-id uint) (beneficiary principal))
  (match (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary })
    beneficiary-info (get active beneficiary-info)
    false
  )
)

(define-read-only (calculate-beneficiary-payout (policy-id uint) (beneficiary principal) (total-amount uint))
  (match (map-get? beneficiaries { policy-id: policy-id, beneficiary: beneficiary })
    beneficiary-info 
      (if (get active beneficiary-info)
        (some (/ (* total-amount (get percentage beneficiary-info)) u100))
        none
      )
    none
  )
)

(define-read-only (get-remaining-coverage (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some (- (get coverage policy) (get total-claimed policy)))
    none
  )
)

(define-read-only (get-policy-claim-history (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some {
      total-coverage: (get coverage policy),
      total-claimed: (get total-claimed policy),
      remaining-coverage: (- (get coverage policy) (get total-claimed policy)),
      claim-count: (get claim-count policy),
      coverage-exhausted: (>= (get total-claimed policy) (get coverage policy))
    })
    none
  )
)

(define-read-only (can-submit-claim (policy-id uint) (amount uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (let (
      (remaining-coverage (- (get coverage policy) (get total-claimed policy)))
    )
      (and
        (get active policy)
        (<= stacks-block-height (get end-block policy))
        (> remaining-coverage u0)
        (<= amount remaining-coverage)
        (> amount u0)
      )
    )
    false
  )
)

(define-read-only (get-coverage-utilization (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (some (/ (* (get total-claimed policy) u100) (get coverage policy)))
    none
  )
)