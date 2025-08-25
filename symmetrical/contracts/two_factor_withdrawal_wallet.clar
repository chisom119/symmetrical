;; Two-Factor Withdrawal Wallet Smart Contract
;; Requires owner confirmation + second factor verification for withdrawals

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-withdrawal-not-found (err u103))
(define-constant err-already-confirmed (err u104))
(define-constant err-not-confirmed (err u105))
(define-constant err-invalid-second-factor (err u106))
(define-constant err-withdrawal-expired (err u107))

;; Data Variables
(define-data-var wallet-balance uint u0)
(define-data-var withdrawal-counter uint u0)
(define-data-var second-factor-enabled bool true)

;; Data Maps
;; Track pending withdrawals that require second factor
(define-map pending-withdrawals
    uint ;; withdrawal-id
    {
        owner: principal,
        amount: uint,
        recipient: principal,
        block-height: uint,
        owner-confirmed: bool,
        second-factor-confirmed: bool,
        expiry-height: uint
    }
)

;; Store authorized second-factor validators (could be other contracts or principals)
(define-map authorized-validators principal bool)

;; Read-only functions
(define-read-only (get-balance)
    (var-get wallet-balance))

(define-read-only (get-withdrawal-details (withdrawal-id uint))
    (map-get? pending-withdrawals withdrawal-id))

(define-read-only (is-validator (validator principal))
    (default-to false (map-get? authorized-validators validator)))

(define-read-only (get-withdrawal-counter)
    (var-get withdrawal-counter))

;; Public functions

;; Deposit STX to the wallet
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set wallet-balance (+ (var-get wallet-balance) amount))
        (ok amount)))

;; Step 1: Initiate withdrawal (owner confirmation)
(define-public (initiate-withdrawal (amount uint) (recipient principal))
    (let
        (
            (current-balance (var-get wallet-balance))
            (withdrawal-id (+ (var-get withdrawal-counter) u1))
            (current-height block-height)
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= current-balance amount) err-insufficient-balance)
        
        ;; Create pending withdrawal
        (map-set pending-withdrawals withdrawal-id
            {
                owner: tx-sender,
                amount: amount,
                recipient: recipient,
                block-height: current-height,
                owner-confirmed: true,
                second-factor-confirmed: false,
                expiry-height: (+ current-height u144) ;; ~24 hours at ~10min blocks
            })
        
        (var-set withdrawal-counter withdrawal-id)
        (ok withdrawal-id)))

;; Step 2: Second factor confirmation
;; This can be called by authorized validators or simulate off-chain verification
(define-public (confirm-second-factor (withdrawal-id uint) (verification-code (string-ascii 32)))
    (let
        (
            (withdrawal-data (unwrap! (map-get? pending-withdrawals withdrawal-id) err-withdrawal-not-found))
            (current-height block-height)
        )
        ;; Check if withdrawal exists and hasn't expired
        (asserts! (<= current-height (get expiry-height withdrawal-data)) err-withdrawal-expired)
        (asserts! (get owner-confirmed withdrawal-data) err-not-confirmed)
        (asserts! (not (get second-factor-confirmed withdrawal-data)) err-already-confirmed)
        
        ;; Simulate second factor verification
        ;; In production, this could integrate with external oracles or authorized contracts
        (asserts! (verify-second-factor verification-code) err-invalid-second-factor)
        
        ;; Update withdrawal with second factor confirmation
        (map-set pending-withdrawals withdrawal-id
            (merge withdrawal-data { second-factor-confirmed: true }))
        
        (ok true)))

;; Execute withdrawal after both confirmations
(define-public (execute-withdrawal (withdrawal-id uint))
    (let
        (
            (withdrawal-data (unwrap! (map-get? pending-withdrawals withdrawal-id) err-withdrawal-not-found))
            (amount (get amount withdrawal-data))
            (recipient (get recipient withdrawal-data))
            (current-height block-height)
        )
        ;; Verify all conditions
        (asserts! (<= current-height (get expiry-height withdrawal-data)) err-withdrawal-expired)
        (asserts! (get owner-confirmed withdrawal-data) err-not-confirmed)
        (asserts! (get second-factor-confirmed withdrawal-data) err-not-confirmed)
        (asserts! (>= (var-get wallet-balance) amount) err-insufficient-balance)
        
        ;; Execute the withdrawal
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (var-set wallet-balance (- (var-get wallet-balance) amount))
        
        ;; Clean up - remove the processed withdrawal
        (map-delete pending-withdrawals withdrawal-id)
        
        (ok amount)))

;; Emergency cancel withdrawal (owner only)
(define-public (cancel-withdrawal (withdrawal-id uint))
    (let
        (
            (withdrawal-data (unwrap! (map-get? pending-withdrawals withdrawal-id) err-withdrawal-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get owner withdrawal-data) tx-sender) err-owner-only)
        
        (map-delete pending-withdrawals withdrawal-id)
        (ok true)))

;; Admin functions

;; Add authorized validator for second factor
(define-public (add-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-validators validator true)
        (ok true)))

;; Remove authorized validator
(define-public (remove-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete authorized-validators validator)
        (ok true)))

;; Toggle second factor requirement
(define-public (toggle-second-factor)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set second-factor-enabled (not (var-get second-factor-enabled)))
        (ok (var-get second-factor-enabled))))

;; Private functions

;; Simulate second factor verification
;; In production, this could integrate with:
;; - External oracle for SMS/email verification
;; - Hardware security modules
;; - Multi-signature schemes
;; - Time-based one-time passwords (TOTP)
(define-private (verify-second-factor (code (string-ascii 32)))
    (or
        (is-eq code "AUTH123")
        (is-eq code "VERIFY456")
        (is-eq code "SECURE789")
        (is-eq code "TESTCODE123")
    ))

;; Alternative: Validator-based second factor confirmation
(define-public (validator-confirm-second-factor (withdrawal-id uint))
    (let
        (
            (withdrawal-data (unwrap! (map-get? pending-withdrawals withdrawal-id) err-withdrawal-not-found))
            (current-height block-height)
        )
        ;; Only authorized validators can confirm
        (asserts! (is-validator tx-sender) err-owner-only)
        (asserts! (<= current-height (get expiry-height withdrawal-data)) err-withdrawal-expired)
        (asserts! (get owner-confirmed withdrawal-data) err-not-confirmed)
        (asserts! (not (get second-factor-confirmed withdrawal-data)) err-already-confirmed)
        
        ;; Update withdrawal with validator confirmation
        (map-set pending-withdrawals withdrawal-id
            (merge withdrawal-data { second-factor-confirmed: true }))
        
        (ok true)))

;; Initialize contract
(begin
    (var-set wallet-balance u0)
    (var-set withdrawal-counter u0)
    ;; Add contract owner as initial validator
    (map-set authorized-validators contract-owner true))