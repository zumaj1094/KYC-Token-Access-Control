(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_KYC_VERIFIED (err u101))
(define-constant ERR_ALREADY_VERIFIED (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_SELF_TRANSFER (err u106))
(define-constant ERR_KYC_REVOKED (err u107))
(define-constant ERR_VESTING_NOT_FOUND (err u108))
(define-constant ERR_VESTING_ALREADY_EXISTS (err u109))
(define-constant ERR_TOKENS_NOT_VESTED (err u110))
(define-constant ERR_INVALID_VESTING_PARAMS (err u111))
(define-constant ERR_INSUFFICIENT_STAKE_BALANCE (err u112))
(define-constant ERR_STAKE_NOT_FOUND (err u113))
(define-constant ERR_STAKE_STILL_LOCKED (err u114))
(define-constant ERR_INVALID_STAKE_PERIOD (err u115))
(define-constant ERR_ACCOUNT_FROZEN (err u116))
(define-constant ERR_ACCOUNT_NOT_FROZEN (err u117))
(define-constant ERR_CANNOT_FREEZE_OWNER (err u118))

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

(define-map vesting-schedules 
  principal 
  {
    total-amount: uint,
    start-block: uint,
    vesting-period: uint,
    released-amount: uint,
    cliff-period: uint
  }
)

(define-map token-stakes 
  principal 
  {
    staked-amount: uint,
    stake-start-block: uint,
    stake-period: uint,
    reward-rate: uint
  }
)

(define-map frozen-accounts 
  principal 
  {
    frozen-at: uint,
    reason: (string-ascii 128),
    frozen-by: principal
  }
)

(define-data-var total-supply uint u0)
(define-data-var kyc-required-for-transfers bool true)
(define-data-var minimum-verification-level uint u1)
(define-data-var total-staked uint u0)
(define-data-var base-reward-rate uint u100)

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
    (asserts! (is-none (map-get? frozen-accounts tx-sender)) ERR_ACCOUNT_FROZEN)
    (asserts! (is-none (map-get? frozen-accounts recipient)) ERR_ACCOUNT_FROZEN)
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

(define-public (create-vesting-schedule (beneficiary principal) (total-amount uint) (vesting-period uint) (cliff-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> vesting-period u0) ERR_INVALID_VESTING_PARAMS)
    (asserts! (<= cliff-period vesting-period) ERR_INVALID_VESTING_PARAMS)
    (asserts! (is-none (map-get? vesting-schedules beneficiary)) ERR_VESTING_ALREADY_EXISTS)
    (asserts! (default-to false (map-get? kyc-status beneficiary)) ERR_NOT_KYC_VERIFIED)
    (map-set vesting-schedules beneficiary {
      total-amount: total-amount,
      start-block: stacks-block-height,
      vesting-period: vesting-period,
      released-amount: u0,
      cliff-period: cliff-period
    })
    (ok true)
  )
)

(define-public (release-vested-tokens)
  (let (
    (vesting-schedule (map-get? vesting-schedules tx-sender))
    (releasable-amount (get-releasable-amount tx-sender))
  )
    (asserts! (is-some vesting-schedule) ERR_VESTING_NOT_FOUND)
    (asserts! (> releasable-amount u0) ERR_TOKENS_NOT_VESTED)
    (let (
      (schedule (unwrap-panic vesting-schedule))
      (new-released-amount (+ (get released-amount schedule) releasable-amount))
    )
      (try! (ft-mint? kyc-token releasable-amount tx-sender))
      (map-set token-balances tx-sender 
        (+ (default-to u0 (map-get? token-balances tx-sender)) releasable-amount))
      (map-set vesting-schedules tx-sender (merge schedule {released-amount: new-released-amount}))
      (var-set total-supply (+ (var-get total-supply) releasable-amount))
      (ok releasable-amount)
    )
  )
)

(define-read-only (get-vesting-schedule (beneficiary principal))
  (map-get? vesting-schedules beneficiary)
)

(define-read-only (get-releasable-amount (beneficiary principal))
  (let (
    (vesting-schedule (map-get? vesting-schedules beneficiary))
  )
    (if (is-some vesting-schedule)
      (let (
        (schedule (unwrap-panic vesting-schedule))
        (current-block stacks-block-height)
        (start-block (get start-block schedule))
        (vesting-period (get vesting-period schedule))
        (cliff-period (get cliff-period schedule))
        (total-amount (get total-amount schedule))
        (released-amount (get released-amount schedule))
        (elapsed-blocks (- current-block start-block))
      )
        (if (< elapsed-blocks cliff-period)
          u0
          (let (
            (vested-amount (if (>= elapsed-blocks vesting-period)
                            total-amount
                            (/ (* total-amount elapsed-blocks) vesting-period)
                          ))
          )
            (if (> vested-amount released-amount)
              (- vested-amount released-amount)
              u0
            )
          )
        )
      )
      u0
    )
  )
)

(define-read-only (get-vesting-info (beneficiary principal))
  (let (
    (vesting-schedule (map-get? vesting-schedules beneficiary))
  )
    (if (is-some vesting-schedule)
      (let (
        (schedule (unwrap-panic vesting-schedule))
        (releasable (get-releasable-amount beneficiary))
        (current-block stacks-block-height)
        (start-block (get start-block schedule))
        (vesting-period (get vesting-period schedule))
        (cliff-period (get cliff-period schedule))
        (total-amount (get total-amount schedule))
        (released-amount (get released-amount schedule))
        (elapsed-blocks (- current-block start-block))
      )
        (some {
          total-amount: total-amount,
          released-amount: released-amount,
          releasable-amount: releasable,
          remaining-amount: (- total-amount released-amount),
          start-block: start-block,
          vesting-period: vesting-period,
          cliff-period: cliff-period,
          elapsed-blocks: elapsed-blocks,
          is-cliff-passed: (>= elapsed-blocks cliff-period),
          is-fully-vested: (>= elapsed-blocks vesting-period)
        })
      )
      none
    )
  )
)

(define-public (stake-tokens (amount uint) (stake-period uint))
  (let (
    (user-balance (default-to u0 (map-get? token-balances tx-sender)))
    (existing-stake (map-get? token-stakes tx-sender))
    (reward-rate (var-get base-reward-rate))
  )
    (asserts! (default-to false (map-get? kyc-status tx-sender)) ERR_NOT_KYC_VERIFIED)
    (asserts! (is-none (map-get? frozen-accounts tx-sender)) ERR_ACCOUNT_FROZEN)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= user-balance amount) ERR_INSUFFICIENT_STAKE_BALANCE)
    (asserts! (>= stake-period u144) ERR_INVALID_STAKE_PERIOD)
    (asserts! (is-none existing-stake) ERR_VESTING_ALREADY_EXISTS)
    (try! (ft-burn? kyc-token amount tx-sender))
    (map-set token-balances tx-sender (- user-balance amount))
    (map-set token-stakes tx-sender {
      staked-amount: amount,
      stake-start-block: stacks-block-height,
      stake-period: stake-period,
      reward-rate: reward-rate
    })
    (var-set total-staked (+ (var-get total-staked) amount))
    (var-set total-supply (- (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (unstake-tokens)
  (let (
    (stake-info (map-get? token-stakes tx-sender))
    (rewards (get-pending-rewards tx-sender))
  )
    (asserts! (is-some stake-info) ERR_STAKE_NOT_FOUND)
    (let (
      (stake (unwrap-panic stake-info))
      (current-block stacks-block-height)
      (stake-end-block (+ (get stake-start-block stake) (get stake-period stake)))
      (staked-amount (get staked-amount stake))
      (total-return (+ staked-amount rewards))
    )
      (asserts! (>= current-block stake-end-block) ERR_STAKE_STILL_LOCKED)
      (try! (ft-mint? kyc-token total-return tx-sender))
      (map-set token-balances tx-sender 
        (+ (default-to u0 (map-get? token-balances tx-sender)) total-return))
      (map-delete token-stakes tx-sender)
      (var-set total-staked (- (var-get total-staked) staked-amount))
      (var-set total-supply (+ (var-get total-supply) total-return))
      (ok total-return)
    )
  )
)

(define-public (set-base-reward-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
    (var-set base-reward-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-stake-info (staker principal))
  (let (
    (stake-info (map-get? token-stakes staker))
  )
    (if (is-some stake-info)
      (let (
        (stake (unwrap-panic stake-info))
        (current-block stacks-block-height)
        (stake-start-block (get stake-start-block stake))
        (stake-period (get stake-period stake))
        (stake-end-block (+ stake-start-block stake-period))
        (staked-amount (get staked-amount stake))
        (reward-rate (get reward-rate stake))
        (pending-rewards (get-pending-rewards staker))
        (elapsed-blocks (- current-block stake-start-block))
      )
        (some {
          staked-amount: staked-amount,
          stake-start-block: stake-start-block,
          stake-period: stake-period,
          stake-end-block: stake-end-block,
          reward-rate: reward-rate,
          pending-rewards: pending-rewards,
          elapsed-blocks: elapsed-blocks,
          is-unlocked: (>= current-block stake-end-block),
          total-return: (+ staked-amount pending-rewards)
        })
      )
      none
    )
  )
)

(define-read-only (get-pending-rewards (staker principal))
  (let (
    (stake-info (map-get? token-stakes staker))
  )
    (if (is-some stake-info)
      (let (
        (stake (unwrap-panic stake-info))
        (current-block stacks-block-height)
        (stake-start-block (get stake-start-block stake))
        (stake-period (get stake-period stake))
        (staked-amount (get staked-amount stake))
        (reward-rate (get reward-rate stake))
        (elapsed-blocks (- current-block stake-start-block))
        (effective-period (if (> elapsed-blocks stake-period) stake-period elapsed-blocks))
      )
        (/ (* (* staked-amount reward-rate) effective-period) (* u10000 u144))
      )
      u0
    )
  )
)

(define-read-only (get-staking-stats)
  {
    total-staked: (var-get total-staked),
    base-reward-rate: (var-get base-reward-rate),
    total-supply: (var-get total-supply),
    circulating-supply: (- (var-get total-supply) (var-get total-staked))
  }
)

(define-public (freeze-account (account principal) (reason (string-ascii 128)))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (default-to false (map-get? kyc-verifiers tx-sender))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq account CONTRACT_OWNER)) ERR_CANNOT_FREEZE_OWNER)
    (asserts! (is-none (map-get? frozen-accounts account)) ERR_VESTING_ALREADY_EXISTS)
    (map-set frozen-accounts account {
      frozen-at: stacks-block-height,
      reason: reason,
      frozen-by: tx-sender
    })
    (var-set frozen-accounts-count (+ (var-get frozen-accounts-count) u1))
    (ok true)
  )
)

(define-public (unfreeze-account (account principal))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (default-to false (map-get? kyc-verifiers tx-sender))) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? frozen-accounts account)) ERR_ACCOUNT_NOT_FROZEN)
    (map-delete frozen-accounts account)
    (var-set frozen-accounts-count (- (var-get frozen-accounts-count) u1))
    (ok true)
  )
)

(define-read-only (is-account-frozen (account principal))
  (is-some (map-get? frozen-accounts account))
)

(define-read-only (get-freeze-info (account principal))
  (let (
    (freeze-info (map-get? frozen-accounts account))
  )
    (if (is-some freeze-info)
      (let (
        (info (unwrap-panic freeze-info))
        (current-block stacks-block-height)
        (frozen-at (get frozen-at info))
        (reason (get reason info))
        (frozen-by (get frozen-by info))
        (frozen-duration (- current-block frozen-at))
      )
        (some {
          frozen-at: frozen-at,
          reason: reason,
          frozen-by: frozen-by,
          frozen-duration: frozen-duration,
          current-block: current-block
        })
      )
      none
    )
  )
)

(define-data-var frozen-accounts-count uint u0)

(define-read-only (get-frozen-accounts-count)
  (var-get frozen-accounts-count)
)

(map-set kyc-verifiers CONTRACT_OWNER true)