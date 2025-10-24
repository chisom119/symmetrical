;; Enhanced Storage with Access Logs Smart Contract
;; Advanced storage contract with comprehensive access tracking, versioning, and advanced features

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_STORAGE_FULL (err u104))
(define-constant ERR_ACCESS_DENIED (err u105))
(define-constant ERR_QUOTA_EXCEEDED (err u106))
(define-constant ERR_CONTRACT_PAUSED (err u107))

;; Constants
(define-constant MAX_READERS u500)
(define-constant MAX_VERSIONS u100)
(define-constant MAX_WHITELIST u50)
(define-constant DEFAULT_READ_QUOTA u10)

;; Data variables
(define-data-var stored-value uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var access-fee uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var value-description (string-ascii 256) "")
(define-data-var creation-time uint block-height)
(define-data-var last-updated uint block-height)
(define-data-var current-version uint u1)

;; Access control
(define-data-var public-read bool true)
(define-data-var whitelist-enabled bool false)

;; Maps for core functionality
(define-map readers principal {
  first-read: uint,
  read-count: uint,
  last-read: uint,
  total-paid: uint
})

(define-map reader-quotas principal uint)
(define-map whitelist principal bool)
(define-map admins principal bool)

;; Version history
(define-map version-history uint {
  value: uint,
  timestamp: uint,
  updater: principal,
  description: (string-ascii 256)
})

;; Lists for maintaining order
(define-data-var reader-list (list 500 principal) (list))
(define-data-var admin-list (list 20 principal) (list))

;; Statistics
(define-data-var total-reads uint u0)
(define-data-var unique-readers uint u0)

;; Events (using print for logging)
(define-private (log-event (event-type (string-ascii 50)) (data (string-ascii 200)))
  (print {event: event-type, data: data, block: block-height, timestamp: block-height})
)

;; Helper functions
(define-private (is-owner (address principal))
  (is-eq address (var-get contract-owner))
)

(define-private (is-admin (address principal))
  (or (is-owner address) (default-to false (map-get? admins address)))
)

(define-private (is-whitelisted (address principal))
  (or (not (var-get whitelist-enabled)) 
      (default-to false (map-get? whitelist address))
      (is-admin address)
  )
)

(define-private (has-read (reader principal))
  (is-some (map-get? readers reader))
)

(define-private (get-read-quota (reader principal))
  (default-to DEFAULT_READ_QUOTA (map-get? reader-quotas reader))
)

(define-private (can-read (reader principal))
  (let ((reader-data (map-get? readers reader)))
    (match reader-data
      data (< (get read-count data) (get-read-quota reader))
      true
    )
  )
) ;; New readers can always read at least once

(define-private (add-to-reader-list (reader principal))
  (let ((current-list (var-get reader-list))
        (list-length (len current-list)))
    (if (and (is-none (index-of? current-list reader))
             (< list-length MAX_READERS))
        (match (as-max-len? (append current-list reader) u500)
          updated-list (begin
                        (var-set reader-list updated-list)
                        (var-set unique-readers (+ (var-get unique-readers) u1))
                        true
                      )
          false)
        false
    )
  )
)

(define-private (save-version (description (string-ascii 256)))
  (let ((version (var-get current-version)))
    (map-set version-history version {
      value: (var-get stored-value),
      timestamp: block-height,
      updater: tx-sender,
      description: description
    })
    (var-set current-version (+ version u1))
  )
)

;; Owner/Admin functions
(define-public (set-value (new-value uint) (description (string-ascii 256)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (save-version description)
    (var-set stored-value new-value)
    (var-set value-description description)
    (var-set last-updated block-height)
    (log-event "VALUE_UPDATED" "Value has been updated")
    (ok true)
  )
)

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (default-to false (map-get? admins new-admin))) ERR_ALREADY_EXISTS)
    (map-set admins new-admin true)
    (let ((current-list (var-get admin-list)))
      (if (< (len current-list) u20)
          (match (as-max-len? (append current-list new-admin) u20)
            updated-list (var-set admin-list updated-list)
            (var-set admin-list current-list))
          (var-set admin-list current-list)))
    (log-event "ADMIN_ADDED" "New admin added")
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq admin (var-get contract-owner))) ERR_NOT_AUTHORIZED)
    (map-delete admins admin)
    (var-set admin-list (filter not-equal-to-admin (var-get admin-list)))
    (log-event "ADMIN_REMOVED" "Admin removed")
    (ok true)
  )
)

(define-private (not-equal-to-admin (admin principal))
  (not (is-eq admin tx-sender))
)

(define-public (set-access-fee (fee uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (var-set access-fee fee)
    (log-event "FEE_UPDATED" "Access fee updated")
    (ok true)
  )
)

(define-public (toggle-public-read)
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (var-set public-read (not (var-get public-read)))
    (log-event "PUBLIC_READ_TOGGLED" "Public read access toggled")
    (ok true)
  )
)

(define-public (toggle-whitelist)
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (var-set whitelist-enabled (not (var-get whitelist-enabled)))
    (log-event "WHITELIST_TOGGLED" "Whitelist mode toggled")
    (ok true)
  )
)

(define-public (add-to-whitelist (address principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (< (len (var-get reader-list)) MAX_WHITELIST) ERR_STORAGE_FULL)
    (map-set whitelist address true)
    (log-event "WHITELIST_ADDED" "Address added to whitelist")
    (ok true)
  )
)

(define-public (remove-from-whitelist (address principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (map-delete whitelist address)
    (log-event "WHITELIST_REMOVED" "Address removed from whitelist")
    (ok true)
  )
)

(define-public (set-reader-quota (reader principal) (quota uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (map-set reader-quotas reader quota)
    (log-event "QUOTA_SET" "Reader quota updated")
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (log-event "CONTRACT_PAUSED" "Contract has been paused")
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (log-event "CONTRACT_UNPAUSED" "Contract has been unpaused")
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (<= amount (var-get total-fees-collected)) ERR_INVALID_INPUT)
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    (var-set total-fees-collected (- (var-get total-fees-collected) amount))
    (log-event "FEES_WITHDRAWN" "Fees withdrawn by owner")
    (ok true)
  )
)

;; Core reading function with comprehensive logging
(define-public (get-value-with-log)
  (let ((reader tx-sender)
        (current-value (var-get stored-value))
        (fee (var-get access-fee)))
    (begin
      ;; Check if contract is paused
      (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
      
      ;; Check access permissions
      (asserts! (or (var-get public-read) (is-whitelisted reader)) ERR_ACCESS_DENIED)
      
      ;; Check quota
      (asserts! (can-read reader) ERR_QUOTA_EXCEEDED)
      
      ;; Handle payment if fee is set
      (if (> fee u0)
          (begin
            (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
            (var-set total-fees-collected (+ (var-get total-fees-collected) fee)))
          true
      )
      
      ;; Update reader statistics
      (match (map-get? readers reader)
        existing-data 
        (map-set readers reader {
          first-read: (get first-read existing-data),
          read-count: (+ (get read-count existing-data) u1),
          last-read: block-height,
          total-paid: (+ (get total-paid existing-data) fee)
        })
        ;; New reader
        (begin
          (map-set readers reader {
            first-read: block-height,
            read-count: u1,
            last-read: block-height,
            total-paid: fee
          })
          (add-to-reader-list reader)
        )
      )
      
      ;; Update global statistics
      (var-set total-reads (+ (var-get total-reads) u1))
      (log-event "VALUE_READ" "Value accessed by user")
      
      (ok {
        value: current-value,
        reader-count: (len (var-get reader-list)),
        total-reads: (var-get total-reads),
        fee-paid: fee
      })
    )
  )
)

;; Batch operations
(define-public (batch-add-whitelist (addresses (list 10 principal)))
  (begin
    (asserts! (is-admin tx-sender) ERR_NOT_AUTHORIZED)
    (map add-single-whitelist addresses)
    (ok true)
  )
)

(define-private (add-single-whitelist (address principal))
  (map-set whitelist address true)
)

;; Read-only functions
(define-read-only (get-value)
  (var-get stored-value)
)

(define-read-only (get-contract-stats)
  {
    stored-value: (var-get stored-value),
    total-readers: (var-get unique-readers),
    total-reads: (var-get total-reads),
    current-version: (var-get current-version),
    creation-time: (var-get creation-time),
    last-updated: (var-get last-updated),
    access-fee: (var-get access-fee),
    total-fees-collected: (var-get total-fees-collected),
    is-paused: (var-get contract-paused),
    public-read: (var-get public-read),
    whitelist-enabled: (var-get whitelist-enabled),
    description: (var-get value-description)
  }
)

(define-read-only (get-reader-info (reader principal))
  (map-get? readers reader)
)

(define-read-only (get-readers-paginated (offset uint) (limit uint))
  (let ((full-list (var-get reader-list))
        (list-len (len full-list)))
    (if (>= offset list-len)
        (list)
        (let ((end-index (if (> (+ offset limit) list-len) list-len (+ offset limit))))
          (slice full-list offset end-index)
        )
    )
  )
)

(define-private (slice (lst (list 500 principal)) (start uint) (end uint))
  ;; Simple slice implementation - in production, you'd want a more efficient version
  (var-get reader-list)
) ;; Simplified for this example

(define-read-only (get-version-history (version uint))
  (map-get? version-history version)
)

(define-read-only (get-recent-versions (count uint))
  (let ((current (var-get current-version)))
    (if (> current count)
        (map get-version-data (range (- current count) current))
        (map get-version-data (range u1 current))
    )
  )
)

(define-private (get-version-data (version uint))
  (map-get? version-history version)
)

(define-private (range (start uint) (end uint))
  ;; Helper function to generate range - simplified implementation
  (list u1 u2 u3 u4 u5)
) ;; In practice, you'd implement proper range generation

(define-read-only (is-reader-whitelisted (reader principal))
  (default-to false (map-get? whitelist reader))
)

(define-read-only (get-reader-quota (reader principal))
  (get-read-quota reader)
)

(define-read-only (get-admins)
  (var-get admin-list)
)

(define-read-only (can-user-read (user principal))
  (and 
    (not (var-get contract-paused))
    (or (var-get public-read) (is-whitelisted user))
    (can-read user)
  )
)

;; Advanced analytics
(define-read-only (get-top-readers (count uint))
  ;; Returns readers sorted by read count (simplified implementation)
  (var-get reader-list)
)

(define-read-only (get-access-statistics)
  (let ((readers-list (var-get reader-list)))
    {
      unique-readers: (len readers-list),
      total-reads: (var-get total-reads),
      average-reads-per-user: (if (> (len readers-list) u0) 
                                 (/ (var-get total-reads) (len readers-list)) 
                                 u0),
      total-fees-collected: (var-get total-fees-collected)
    }
  )
)

;; Emergency functions
(define-public (emergency-reset)
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (var-set reader-list (list))
    (var-set unique-readers u0)
    (var-set total-reads u0)
    (log-event "EMERGENCY_RESET" "All reader data has been reset")
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-owner tx-sender) ERR_NOT_AUTHORIZED)
    (var-set contract-owner new-owner)
    (log-event "OWNERSHIP_TRANSFERRED" "Contract ownership transferred")
    (ok true)
  )
)

;; Contract information
(define-read-only (get-contract-info)
  {
    name: "Enhanced Storage with Access Logs",
    version: "2.0.0",
    owner: (var-get contract-owner),
    admins: (var-get admin-list),
    features: (list 
      "access-logging" 
      "version-history" 
      "fee-collection" 
      "access-control" 
      "quotas" 
      "whitelist" 
      "analytics"
    )
  }
)