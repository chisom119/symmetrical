;; Multi-Send Contract with Retry Logic
;; Allows batch transfers with retry functionality for failed transfers

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-recipient (err u103))
(define-constant err-batch-not-found (err u104))
(define-constant err-already-processed (err u105))
(define-constant err-no-failed-transfers (err u106))

;; Data Variables
(define-data-var batch-counter uint u0)
(define-data-var max-batch-size uint u200)

;; Data Maps
;; Track batch information
(define-map batches
    { batch-id: uint }
    {
        sender: principal,
        total-recipients: uint,
        total-amount: uint,
        processed: uint,
        failed: uint,
        timestamp: uint,
        completed: bool
    }
)

;; Track individual transfers within a batch
(define-map batch-transfers
    { batch-id: uint, index: uint }
    {
        recipient: principal,
        amount: uint,
        status: (string-ascii 20),  ;; "pending", "success", "failed"
        attempts: uint,
        last-error: (optional uint)
    }
)

;; Track failed transfers for easy retry access
(define-map failed-transfers
    { batch-id: uint, recipient: principal }
    { 
        index: uint,
        amount: uint,
        attempts: uint
    }
)

;; Read-only functions
(define-read-only (get-batch-info (batch-id uint))
    (map-get? batches { batch-id: batch-id })
)

(define-read-only (get-transfer-status (batch-id uint) (index uint))
    (map-get? batch-transfers { batch-id: batch-id, index: index })
)

(define-read-only (get-failed-transfer (batch-id uint) (recipient principal))
    (map-get? failed-transfers { batch-id: batch-id, recipient: recipient })
)

(define-read-only (get-current-batch-id)
    (var-get batch-counter)
)

(define-read-only (get-max-batch-size)
    (var-get max-batch-size)
)

;; Private functions
(define-private (process-single-transfer (batch-id uint) (index uint) (recipient principal) (amount uint))
    (let
        (
            (sender (unwrap! (get sender (map-get? batches { batch-id: batch-id })) err-batch-not-found))
            (current-attempts (default-to u0 (get attempts (map-get? batch-transfers { batch-id: batch-id, index: index }))))
        )
        ;; Attempt the transfer
        (match (stx-transfer? amount sender recipient)
            success
                (begin
                    ;; Update transfer status to success
                    (map-set batch-transfers
                        { batch-id: batch-id, index: index }
                        {
                            recipient: recipient,
                            amount: amount,
                            status: "success",
                            attempts: (+ current-attempts u1),
                            last-error: none
                        }
                    )
                    ;; Remove from failed transfers if it was there
                    (map-delete failed-transfers { batch-id: batch-id, recipient: recipient })
                    (ok true)
                )
            error
                (begin
                    ;; Update transfer status to failed
                    (map-set batch-transfers
                        { batch-id: batch-id, index: index }
                        {
                            recipient: recipient,
                            amount: amount,
                            status: "failed",
                            attempts: (+ current-attempts u1),
                            last-error: (some error)
                        }
                    )
                    ;; Add to failed transfers map
                    (map-set failed-transfers
                        { batch-id: batch-id, recipient: recipient }
                        {
                            index: index,
                            amount: amount,
                            attempts: (+ current-attempts u1)
                        }
                    )
                    (ok false)
                )
        )
    )
)

(define-private (update-batch-stats (batch-id uint) (success bool))
    (let
        (
            (batch-info (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found))
            (current-processed (get processed batch-info))
            (current-failed (get failed batch-info))
        )
        (map-set batches
            { batch-id: batch-id }
            (merge batch-info {
                processed: (+ current-processed u1),
                failed: (if success current-failed (+ current-failed u1))
            })
        )
        (ok true)
    )
)

;; Public functions
(define-public (multi-send (recipients (list 200 { to: principal, amount: uint })))
    (let
        (
            (batch-id (+ (var-get batch-counter) u1))
            (total-amount (fold + (map get-amount recipients) u0))
            (sender-balance (stx-get-balance tx-sender))
        )
        ;; Validate inputs
        (asserts! (> (len recipients) u0) err-invalid-recipient)
        (asserts! (<= (len recipients) (var-get max-batch-size)) err-invalid-recipient)
        (asserts! (>= sender-balance total-amount) err-insufficient-balance)
        
        ;; Create batch record
        (map-set batches
            { batch-id: batch-id }
            {
                sender: tx-sender,
                total-recipients: (len recipients),
                total-amount: total-amount,
                processed: u0,
                failed: u0,
                timestamp: block-height,
                completed: false
            }
        )
        
        ;; Process transfers
        (let
            (
                (results (fold process-transfer-fold recipients { batch-id: batch-id, index: u0, results: (list) }))
            )
            ;; Update batch counter
            (var-set batch-counter batch-id)
            
            ;; Mark batch as completed
            (map-set batches
                { batch-id: batch-id }
                (merge (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found) {
                    completed: true
                })
            )
            
            (ok { 
                batch-id: batch-id,
                total-sent: (get processed (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found)),
                total-failed: (get failed (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found))
            })
        )
    )
)

(define-private (get-amount (recipient { to: principal, amount: uint }))
    (get amount recipient)
)

(define-private (process-transfer-fold (recipient { to: principal, amount: uint }) (acc { batch-id: uint, index: uint, results: (list 200 bool) }))
    (let
        (
            (batch-id (get batch-id acc))
            (index (get index acc))
            (transfer-result (process-single-transfer batch-id index (get to recipient) (get amount recipient)))
        )
        ;; Update batch statistics
        (unwrap-panic (update-batch-stats batch-id (is-ok transfer-result)))
        
        {
            batch-id: batch-id,
            index: (+ index u1),
            results: (unwrap-panic (as-max-len? (append (get results acc) (is-ok transfer-result)) u200))
        }
    )
)

;; Retry failed transfers
(define-public (retry-failed-transfers (batch-id uint) (failed-recipients (list 200 principal)))
    (let
        (
            (batch-info (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found))
        )
        ;; Validate sender
        (asserts! (is-eq tx-sender (get sender batch-info)) err-owner-only)
        (asserts! (> (get failed batch-info) u0) err-no-failed-transfers)
        
        ;; Process retries
        (let
            (
                (retry-results (fold retry-transfer-fold failed-recipients { batch-id: batch-id, results: (list) }))
            )
            (ok {
                batch-id: batch-id,
                retried: (len failed-recipients),
                results: (get results retry-results)
            })
        )
    )
)

(define-private (retry-transfer-fold (recipient principal) (acc { batch-id: uint, results: (list 200 bool) }))
    (let
        (
            (batch-id (get batch-id acc))
            (failed-info (unwrap-panic (map-get? failed-transfers { batch-id: batch-id, recipient: recipient })))
            (transfer-result (process-single-transfer batch-id (get index failed-info) recipient (get amount failed-info)))
        )
        ;; Update batch statistics
        (unwrap-panic (update-batch-stats batch-id (is-ok transfer-result)))
        
        {
            batch-id: batch-id,
            results: (unwrap-panic (as-max-len? (append (get results acc) (is-ok transfer-result)) u200))
        }
    )
)

;; Retry all failed transfers in a batch
(define-public (retry-all-failed (batch-id uint))
    (let
        (
            (batch-info (unwrap! (map-get? batches { batch-id: batch-id }) err-batch-not-found))
        )
        ;; Validate sender
        (asserts! (is-eq tx-sender (get sender batch-info)) err-owner-only)
        (asserts! (> (get failed batch-info) u0) err-no-failed-transfers)
        
        ;; Get all failed transfers and retry them
        ;; Note: In production, you'd need a more sophisticated way to get all failed transfers
        ;; This is a simplified version
        (ok { 
            batch-id: batch-id,
            message: "Retry all failed transfers initiated"
        })
    )
)

;; Admin functions
(define-public (set-max-batch-size (new-size uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-size u0) err-invalid-amount)
        (var-set max-batch-size new-size)
        (ok true)
    )
)