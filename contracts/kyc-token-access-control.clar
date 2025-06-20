(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_KYC_VERIFIED (err u101))
(define-constant ERR_ALREADY_VERIFIED (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_SELF_TRANSFER (err u106))
(define-constant ERR_KYC_REVOKED (err u107))

(define-fungible-token kyc-token)

(define-map kyc-status principal bool)
(define-map token-balances principal uint)
(define-map kyc-verifiers principal bool)
(define-map user-profiles 
  principal 
  {
    verification-level: uint,
    verified-at: uint,
    country-code: (string-ascii 3)
  }
)

(define-data-var total-supply uint u0)
(define-data-var kyc-required-for-transfers bool true)
(define-data-var minimum-verification-level uint u1)

(define-public (add-kyc-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set kyc-verifiers verifier true)
    (ok true)
  )
)

(define-public (remove-kyc-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete kyc-verifiers verifier)
    (ok true)
  )
)

(define-public (verify-kyc (user principal) (level uint) (country (string-ascii 3)))
  (begin
    (asserts! (default-to false (map-get? kyc-verifiers tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (not (default-to false (map-get? kyc-status user))) ERR_ALREADY_VERIFIED)
    (map-set kyc-status user true)
    (map-set user-profiles user {
      verification-level: level,
      verified-at: stacks-block-height,
      country-code: country
    })
    (ok true)
  )
)

(define-public (revoke-kyc (user principal))
  (begin
    (asserts! (default-to false (map-get? kyc-verifiers tx-sender)) ERR_UNAUTHORIZED)
    (map-set kyc-status user false)
    (ok true)
  )
)

(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (default-to false (map-get? kyc-status recipient)) ERR_NOT_KYC_VERIFIED)
    (try! (ft-mint? kyc-token amount recipient))
    (map-set token-balances recipient 
      (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (transfer-tokens (recipient principal) (amount uint))
  (let (
    (sender-balance (default-to u0 (map-get? token-balances tx-sender)))
    (recipient-balance (default-to u0 (map-get? token-balances recipient)))
  )
    (asserts! (not (is-eq tx-sender recipient)) ERR_SELF_TRANSFER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
    (if (var-get kyc-required-for-transfers)
      (begin
        (asserts! (default-to false (map-get? kyc-status tx-sender)) ERR_NOT_KYC_VERIFIED)
        (asserts! (default-to false (map-get? kyc-status recipient)) ERR_NOT_KYC_VERIFIED)
        true
      )
      true
    )
    (try! (ft-transfer? kyc-token amount tx-sender recipient))
    (map-set token-balances tx-sender (- sender-balance amount))
    (map-set token-balances recipient (+ recipient-balance amount))
    (ok true)
  )
)

(define-public (premium-transfer (recipient principal) (amount uint))
  (let (
    (sender-profile (map-get? user-profiles tx-sender))
    (recipient-profile (map-get? user-profiles recipient))
  )
    (asserts! (is-some sender-profile) ERR_NOT_KYC_VERIFIED)
    (asserts! (is-some recipient-profile) ERR_NOT_KYC_VERIFIED)
    (asserts! (>= (get verification-level (unwrap-panic sender-profile)) u2) ERR_UNAUTHORIZED)
    (asserts! (>= (get verification-level (unwrap-panic recipient-profile)) u2) ERR_UNAUTHORIZED)
    (transfer-tokens recipient amount)
  )
)

(define-public (bulk-transfer (recipients (list 10 {recipient: principal, amount: uint})))
  (begin
    (asserts! (default-to false (map-get? kyc-status tx-sender)) ERR_NOT_KYC_VERIFIED)
    (fold process-bulk-transfer recipients (ok u0))
  )
)

(define-private (process-bulk-transfer 
  (transfer-data {recipient: principal, amount: uint}) 
  (previous-result (response uint uint))
)
  (match previous-result
    success (match (transfer-tokens (get recipient transfer-data) (get amount transfer-data))
              ok (ok (+ success u1))
              err (err err)
            )
    error (err error)
  )
)

(define-public (set-kyc-requirement (required bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set kyc-required-for-transfers required)
    (ok true)
  )
)

(define-public (set-minimum-verification-level (level uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set minimum-verification-level level)
    (ok true)
  )
)

(define-public (upgrade-verification (user principal) (new-level uint))
  (let (
    (current-profile (map-get? user-profiles user))
  )
    (asserts! (default-to false (map-get? kyc-verifiers tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-some current-profile) ERR_NOT_KYC_VERIFIED)
    (map-set user-profiles user (merge (unwrap-panic current-profile) {verification-level: new-level}))
    (ok true)
  )
)

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? token-balances user))
)

(define-read-only (is-kyc-verified (user principal))
  (default-to false (map-get? kyc-status user))
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

(define-read-only (is-kyc-verifier (verifier principal))
  (default-to false (map-get? kyc-verifiers verifier))
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-kyc-requirement)
  (var-get kyc-required-for-transfers)
)

(define-read-only (get-minimum-verification-level)
  (var-get minimum-verification-level)
)

(define-read-only (can-access-premium-features (user principal))
  (let (
    (profile (map-get? user-profiles user))
  )
    (and 
      (is-some profile)
      (>= (get verification-level (unwrap-panic profile)) u2)
      (default-to false (map-get? kyc-status user))
    )
  )
)

(define-read-only (get-contract-info)
  {
    total-supply: (var-get total-supply),
    kyc-required: (var-get kyc-required-for-transfers),
    min-verification-level: (var-get minimum-verification-level),
    contract-owner: CONTRACT_OWNER
  }
)

(map-set kyc-verifiers CONTRACT_OWNER true)