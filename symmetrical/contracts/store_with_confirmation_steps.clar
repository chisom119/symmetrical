;; Advanced Two-Step Confirmation Storage Contract
;; Enhanced version with multiple data types, time locks, voting, and audit features

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-CONFIRMED (err u101))
(define-constant ERR-NO-PENDING-UPDATE (err u102))
(define-constant ERR-CANNOT-CONFIRM-OWN-PROPOSAL (err u103))
(define-constant ERR-INVALID-ADMIN (err u104))
(define-constant ERR-UPDATE-EXPIRED (err u105))
(define-constant ERR-TIME-LOCK-ACTIVE (err u106))
(define-constant ERR-INSUFFICIENT-CONFIRMATIONS (err u107))
(define-constant ERR-INVALID-DATA-TYPE (err u108))
(define-constant ERR-EMERGENCY-PAUSE-ACTIVE (err u109))
(define-constant ERR-INVALID-THRESHOLD (err u110))
(define-constant ERR-BATCH-LIMIT-EXCEEDED (err u111))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-BATCH-SIZE u10)
(define-constant DEFAULT-EXPIRY-BLOCKS u1440) ;; ~24 hours at 1 min/block
(define-constant MIN-TIME-LOCK u144) ;; ~2.4 hours minimum
(define-constant MAX-ADMINS u20)

;; Contract configuration
(define-data-var emergency-pause bool false)
(define-data-var confirmation-threshold uint u2)
(define-data-var time-lock-duration uint u0)
(define-data-var update-expiry-blocks uint DEFAULT-EXPIRY-BLOCKS)
(define-data-var allow-batch-updates bool false)

;; Storage for different data types
(define-data-var stored-uint uint u0)
(define-data-var stored-string (string-ascii 256) "")
(define-data-var stored-bool bool false)
(define-map stored-principals principal bool)
(define-map stored-tuples uint { value: uint, metadata: (string-ascii 100) })

;; Admin management with roles
(define-map admins principal { 
  active: bool, 
  role: (string-ascii 20),
  added-at: uint,
  added-by: principal
})

(define-data-var admin-count uint u1)

;; Enhanced pending update structure
(define-map pending-updates 
  uint 
  {
    data-type: (string-ascii 20),
    uint-value: (optional uint),
    string-value: (optional (string-ascii 256)),
    bool-value: (optional bool),
    principal-value: (optional principal),
    tuple-id: (optional uint),
    proposer: principal,
    confirmations: (list 10 principal),
    confirmation-count: uint,
    block-height: uint,
    expires-at: uint,
    time-lock-ends: (optional uint),
    priority: uint,
    reason: (string-ascii 200),
    batch-id: (optional uint)
  }
)

;; Batch operations
(define-map batch-operations uint {
  updates: (list 10 uint),
  status: (string-ascii 20),
  created-by: principal,
  created-at: uint
})

(define-data-var batch-counter uint u0)

;; Counters and tracking
(define-data-var update-counter uint u0)
(define-data-var tuple-counter uint u0)
(define-map active-updates uint bool)

;; Audit trail
(define-map audit-log uint {
  action: (string-ascii 50),
  actor: principal,
  target-update: (optional uint),
  timestamp: uint,
  details: (string-ascii 200)
})
(define-data-var audit-counter uint u0)

;; Time lock management
(define-map time-locked-updates uint uint) ;; update-id -> unlock-block

;; Initialize contract
(map-set admins CONTRACT-OWNER { 
  active: true, 
  role: "owner",
  added-at: block-height,
  added-by: CONTRACT-OWNER
})

;; Helper functions (defined early to avoid resolution issues)
(define-private (is-active-admin (address principal))
  (match (map-get? admins address)
    admin-data (get active admin-data)
    false
  )
)

(define-private (is-owner-or-senior-admin (address principal))
  (or
    (is-eq address CONTRACT-OWNER)
    (match (map-get? admins address)
      admin-data 
      (and 
        (get active admin-data)
        (or 
          (is-eq (get role admin-data) "senior")
          (is-eq (get role admin-data) "owner")
        )
      )
      false
    )
  )
)

;; Audit logging helper
(define-private (log-action (action (string-ascii 50)) (target (optional uint)) (details (string-ascii 200)))
  (let ((log-id (+ (var-get audit-counter) u1)))
    (begin
      (map-set audit-log log-id {
        action: action,
        actor: tx-sender,
        target-update: target,
        timestamp: block-height,
        details: details
      })
      (var-set audit-counter log-id)
      true
    )
  )
)

;; Enhanced admin management
(define-public (add-admin (new-admin principal) (role (string-ascii 20)))
  (begin
    (asserts! (is-owner-or-senior-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (< (var-get admin-count) MAX-ADMINS) ERR-INVALID-ADMIN)
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-PAUSE-ACTIVE)
    
    (map-set admins new-admin { 
      active: true, 
      role: role,
      added-at: block-height,
      added-by: tx-sender
    })
    (var-set admin-count (+ (var-get admin-count) u1))
    (log-action "admin-added" none (concat "Added admin with role: " role))
    (ok true)
  )
)

(define-public (deactivate-admin (admin principal))
  (begin
    (asserts! (is-owner-or-senior-admin tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq admin CONTRACT-OWNER)) ERR-INVALID-ADMIN)
    
    (match (map-get? admins admin)
      admin-data
      (begin
        (map-set admins admin (merge admin-data { active: false }))
        (var-set admin-count (- (var-get admin-count) u1))
        (log-action "admin-deactivated" none "Admin deactivated")
        (ok true)
      )
      ERR-INVALID-ADMIN
    )
  )
)

;; Emergency controls
(define-public (activate-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-pause true)
    (log-action "emergency-pause" none "Contract paused")
    (ok true)
  )
)

(define-public (deactivate-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-pause false)
    (log-action "emergency-unpause" none "Contract unpaused")
    (ok true)
  )
)

;; Configuration management
(define-public (set-confirmation-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-threshold u2) (<= new-threshold u10)) ERR-INVALID-THRESHOLD)
    (var-set confirmation-threshold new-threshold)
    (ok true)
  )
)

(define-public (set-time-lock-duration (duration uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq duration u0) (>= duration MIN-TIME-LOCK)) ERR-TIME-LOCK-ACTIVE)
    (var-set time-lock-duration duration)
    (ok true)
  )
)

;; Internal proposal helper (defined before its usage)
(define-private (propose-update-internal 
  (data-type (string-ascii 20))
  (uint-val (optional uint))
  (string-val (optional (string-ascii 256)))
  (bool-val (optional bool))
  (principal-val (optional principal))
  (tuple-id (optional uint))
  (priority uint)
  (reason (string-ascii 200))
  (batch-id (optional uint))
)
  (let
    (
      (counter (var-get update-counter))
      (new-counter (+ counter u1))
      (expires-at (+ block-height (var-get update-expiry-blocks)))
      (time-lock-end 
        (if (> (var-get time-lock-duration) u0)
          (some (+ block-height (var-get time-lock-duration)))
          none
        )
      )
    )
    (begin
      (asserts! (is-active-admin tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-PAUSE-ACTIVE)
      
      (map-set pending-updates new-counter {
        data-type: data-type,
        uint-value: uint-val,
        string-value: string-val,
        bool-value: bool-val,
        principal-value: principal-val,
        tuple-id: tuple-id,
        proposer: tx-sender,
        confirmations: (list tx-sender),
        confirmation-count: u1,
        block-height: block-height,
        expires-at: expires-at,
        time-lock-ends: time-lock-end,
        priority: priority,
        reason: reason,
        batch-id: batch-id
      })
      
      (map-set active-updates new-counter true)
      (var-set update-counter new-counter)
      
      (if (is-some time-lock-end)
        (map-set time-locked-updates new-counter (unwrap-panic time-lock-end))
        true
      )
      
      (log-action "proposal-created" (some new-counter) reason)
      (ok new-counter)
    )
  )
)

;; Enhanced proposal system with different data types
(define-public (propose-uint-update (new-value uint) (priority uint) (reason (string-ascii 200)))
  (propose-update-internal "uint" (some new-value) none none none none priority reason none)
)

(define-public (propose-string-update (new-value (string-ascii 256)) (priority uint) (reason (string-ascii 200)))
  (propose-update-internal "string" none (some new-value) none none none priority reason none)
)

(define-public (propose-bool-update (new-value bool) (priority uint) (reason (string-ascii 200)))
  (propose-update-internal "bool" none none (some new-value) none none priority reason none)
)

(define-public (propose-principal-update (new-value principal) (priority uint) (reason (string-ascii 200)))
  (propose-update-internal "principal" none none none (some new-value) none priority reason none)
)

(define-public (propose-tuple-update (value uint) (metadata (string-ascii 100)) (priority uint) (reason (string-ascii 200)))
  (let ((tuple-id (+ (var-get tuple-counter) u1)))
    (begin
      (var-set tuple-counter tuple-id)
      (propose-update-internal "tuple" none none none none (some tuple-id) priority reason none)
    )
  )
)



;; Execute update when threshold is met (defined before confirm-update)
(define-private (execute-update (update-id uint) (pending-update (tuple 
  (data-type (string-ascii 20))
  (uint-value (optional uint))
  (string-value (optional (string-ascii 256)))
  (bool-value (optional bool))
  (principal-value (optional principal))
  (tuple-id (optional uint))
  (proposer principal)
  (confirmations (list 10 principal))
  (confirmation-count uint)
  (block-height uint)
  (expires-at uint)
  (time-lock-ends (optional uint))
  (priority uint)
  (reason (string-ascii 200))
  (batch-id (optional uint))
)))
  (begin
    (if (is-eq (get data-type pending-update) "uint")
      (var-set stored-uint (unwrap-panic (get uint-value pending-update)))
      (if (is-eq (get data-type pending-update) "string")
        (var-set stored-string (unwrap-panic (get string-value pending-update)))
        (if (is-eq (get data-type pending-update) "bool")
          (var-set stored-bool (unwrap-panic (get bool-value pending-update)))
          (if (is-eq (get data-type pending-update) "principal")
            (map-set stored-principals (unwrap-panic (get principal-value pending-update)) true)
            (if (is-eq (get data-type pending-update) "tuple")
              (match (get tuple-id pending-update)
                tid (map-set stored-tuples tid { value: (unwrap-panic (get uint-value pending-update)), metadata: "executed" })
                false
              )
              false
            )
          )
        )
      )
    )
    
    (map-delete active-updates update-id)
    (map-delete time-locked-updates update-id)
    (log-action "update-executed" (some update-id) (get reason pending-update))
    (ok true)
  )
)

;; Enhanced confirmation system
(define-public (confirm-update (update-id uint))
  (match (map-get? pending-updates update-id)
    pending-update
    (begin
      (asserts! (is-active-admin tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-PAUSE-ACTIVE)
      (asserts! (not (is-eq tx-sender (get proposer pending-update))) ERR-CANNOT-CONFIRM-OWN-PROPOSAL)
      (asserts! (<= block-height (get expires-at pending-update)) ERR-UPDATE-EXPIRED)
      (asserts! (default-to true (map-get? active-updates update-id)) ERR-NO-PENDING-UPDATE)
      
      ;; Check if already confirmed by this admin
      (asserts! (is-none (index-of (get confirmations pending-update) tx-sender)) ERR-ALREADY-CONFIRMED)
      
      ;; Check time lock
      (match (get time-lock-ends pending-update)
        time-lock-end
        (asserts! (>= block-height time-lock-end) ERR-TIME-LOCK-ACTIVE)
        true
      )
      
      (let
        (
          (new-confirmations (unwrap-panic (as-max-len? (append (get confirmations pending-update) tx-sender) u10)))
          (new-count (+ (get confirmation-count pending-update) u1))
          (threshold (var-get confirmation-threshold))
        )
        (begin
          (map-set pending-updates update-id 
            (merge pending-update {
              confirmations: new-confirmations,
              confirmation-count: new-count
            })
          )
          
          (if (>= new-count threshold)
            (execute-update update-id pending-update)
            (begin
              (log-action "update-confirmed" (some update-id) "Confirmation added")
              (ok false) ;; Not yet executed
            )
          )
        )
      )
    )
    ERR-NO-PENDING-UPDATE
  )
)



;; Batch operations
(define-public (create-batch (update-ids (list 10 uint)))
  (let ((batch-id (+ (var-get batch-counter) u1)))
    (begin
      (asserts! (is-active-admin tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (var-get allow-batch-updates) ERR-NOT-AUTHORIZED)
      (asserts! (<= (len update-ids) MAX-BATCH-SIZE) ERR-BATCH-LIMIT-EXCEEDED)
      
      (map-set batch-operations batch-id {
        updates: update-ids,
        status: "pending",
        created-by: tx-sender,
        created-at: block-height
      })
      
      (var-set batch-counter batch-id)
      (log-action "batch-created" none "Batch operation created")
      (ok batch-id)
    )
  )
)



;; Enhanced read functions
(define-read-only (get-stored-uint) (var-get stored-uint))
(define-read-only (get-stored-string) (var-get stored-string))
(define-read-only (get-stored-bool) (var-get stored-bool))
(define-read-only (get-stored-principal (key principal)) (map-get? stored-principals key))
(define-read-only (get-stored-tuple (id uint)) (map-get? stored-tuples id))

(define-read-only (get-pending-update (update-id uint))
  (map-get? pending-updates update-id)
)

(define-read-only (get-active-update (update-id uint))
  (map-get? active-updates update-id)
)

(define-read-only (get-admin-info (admin principal))
  (map-get? admins admin)
)

(define-read-only (get-audit-entry (log-id uint))
  (map-get? audit-log log-id)
)

(define-read-only (get-contract-config)
  {
    emergency-pause: (var-get emergency-pause),
    confirmation-threshold: (var-get confirmation-threshold),
    time-lock-duration: (var-get time-lock-duration),
    update-expiry-blocks: (var-get update-expiry-blocks),
    allow-batch-updates: (var-get allow-batch-updates),
    admin-count: (var-get admin-count)
  }
)

(define-read-only (get-comprehensive-status)
  {
    config: (get-contract-config),
    counters: {
      update-counter: (var-get update-counter),
      audit-counter: (var-get audit-counter),
      batch-counter: (var-get batch-counter),
      tuple-counter: (var-get tuple-counter)
    },
    storage: {
      uint-value: (var-get stored-uint),
      string-value: (var-get stored-string),
      bool-value: (var-get stored-bool)
    }
  }
)