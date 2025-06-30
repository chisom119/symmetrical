;; Governance Smart Contract
;; Provides community governance for contract parameters

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PROPOSAL (err u103))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u104))
(define-constant ERR_ALREADY_VOTED (err u105))
(define-constant ERR_VOTING_PERIOD_ENDED (err u106))
(define-constant ERR_INSUFFICIENT_VOTES (err u107))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u108))

;; Proposal types
(define-constant PROPOSAL_TYPE_FEE_CHANGE u1)
(define-constant PROPOSAL_TYPE_POLICY_CHANGE u2)
(define-constant PROPOSAL_TYPE_PARAMETER_CHANGE u3)

;; Proposal status
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_PASSED u2)
(define-constant STATUS_REJECTED u3)
(define-constant STATUS_EXECUTED u4)

;; Governance parameters
(define-data-var voting-period uint u1008) ;; ~1 week in blocks
(define-data-var quorum-threshold uint u1000) ;; Minimum votes needed
(define-data-var approval-threshold uint u6000) ;; 60% approval needed (out of 10000)
(define-data-var proposal-fee uint u1000000) ;; 1 STX fee to create proposal

;; Data structures
(define-map admins principal bool)
(define-map proposals 
  uint 
  {
    id: uint,
    proposer: principal,
    proposal-type: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-contract: (optional principal),
    function-name: (optional (string-ascii 50)),
    parameters: (optional (string-ascii 200)),
    votes-for: uint,
    votes-against: uint,
    status: uint,
    created-at: uint,
    voting-ends-at: uint
  }
)

(define-map votes {proposal-id: uint, voter: principal} bool)
(define-map voter-weights principal uint)

;; Counters
(define-data-var proposal-counter uint u0)

;; Initialize contract owner as admin
(map-set admins CONTRACT_OWNER true)
(map-set voter-weights CONTRACT_OWNER u100) ;; Owner gets 100 voting weight

;; Read-only functions

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (get-voter-weight (voter principal))
  (default-to u1 (map-get? voter-weights voter))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (get-governance-params)
  {
    voting-period: (var-get voting-period),
    quorum-threshold: (var-get quorum-threshold),
    approval-threshold: (var-get approval-threshold),
    proposal-fee: (var-get proposal-fee)
  }
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (get status proposal)
    u0
  )
)

(define-read-only (calculate-vote-result (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal 
    (let 
      (
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-rate (if (> total-votes u0)
                        (/ (* (get votes-for proposal) u10000) total-votes)
                        u0))
      )
      {
        total-votes: total-votes,
        approval-rate: approval-rate,
        meets-quorum: (>= total-votes (var-get quorum-threshold)),
        meets-approval: (>= approval-rate (var-get approval-threshold))
      }
    )
    {total-votes: u0, approval-rate: u0, meets-quorum: false, meets-approval: false}
  )
)

;; Admin functions

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (ok (map-set admins new-admin true))
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq admin CONTRACT_OWNER)) ERR_UNAUTHORIZED) ;; Cannot remove contract owner
    (ok (map-delete admins admin))
  )
)

(define-public (set-voter-weight (voter principal) (weight uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (ok (map-set voter-weights voter weight))
  )
)

(define-public (update-governance-params 
  (new-voting-period uint)
  (new-quorum-threshold uint) 
  (new-approval-threshold uint)
  (new-proposal-fee uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set voting-period new-voting-period)
    (var-set quorum-threshold new-quorum-threshold)
    (var-set approval-threshold new-approval-threshold)
    (var-set proposal-fee new-proposal-fee)
    (ok true)
  )
)

;; Proposal functions

(define-public (create-proposal 
  (proposal-type uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-contract (optional principal))
  (function-name (optional (string-ascii 50)))
  (parameters (optional (string-ascii 200))))
  (let 
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (current-block block-height)
      (voting-ends-at (+ current-block (var-get voting-period)))
    )
    ;; Charge proposal fee
    (try! (stx-transfer? (var-get proposal-fee) tx-sender CONTRACT_OWNER))
    
    ;; Validate proposal type
    (asserts! (or (is-eq proposal-type PROPOSAL_TYPE_FEE_CHANGE)
                  (or (is-eq proposal-type PROPOSAL_TYPE_POLICY_CHANGE)
                      (is-eq proposal-type PROPOSAL_TYPE_PARAMETER_CHANGE)))
              ERR_INVALID_PROPOSAL)
    
    ;; Create proposal
    (map-set proposals proposal-id
      {
        id: proposal-id,
        proposer: tx-sender,
        proposal-type: proposal-type,
        title: title,
        description: description,
        target-contract: target-contract,
        function-name: function-name,
        parameters: parameters,
        votes-for: u0,
        votes-against: u0,
        status: STATUS_ACTIVE,
        created-at: current-block,
        voting-ends-at: voting-ends-at
      }
    )
    
    ;; Update counter
    (var-set proposal-counter proposal-id)
    
    (print {
      event: "proposal-created",
      proposal-id: proposal-id,
      proposer: tx-sender,
      title: title
    })
    
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let 
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
      (voter-weight (get-voter-weight tx-sender))
      (current-block block-height)
    )
    ;; Check if proposal is active
    (asserts! (is-eq (get status proposal) STATUS_ACTIVE) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Check if voting period is still active
    (asserts! (<= current-block (get voting-ends-at proposal)) ERR_VOTING_PERIOD_ENDED)
    
    ;; Check if user hasn't voted yet
    (asserts! (not (has-voted proposal-id tx-sender)) ERR_ALREADY_VOTED)
    
    ;; Record vote
    (map-set votes {proposal-id: proposal-id, voter: tx-sender} support)
    
    ;; Update vote counts
    (if support
      (map-set proposals proposal-id
        (merge proposal {votes-for: (+ (get votes-for proposal) voter-weight)})
      )
      (map-set proposals proposal-id
        (merge proposal {votes-against: (+ (get votes-against proposal) voter-weight)})
      )
    )
    
    (print {
      event: "vote-cast",
      proposal-id: proposal-id,
      voter: tx-sender,
      support: support,
      weight: voter-weight
    })
    
    (ok true)
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
      (vote-result (calculate-vote-result proposal-id))
      (current-block block-height)
    )
    ;; Check if proposal is active
    (asserts! (is-eq (get status proposal) STATUS_ACTIVE) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Check if voting period has ended
    (asserts! (> current-block (get voting-ends-at proposal)) ERR_VOTING_PERIOD_ENDED)
    
    (let 
      (
        (new-status (if (and (get meets-quorum vote-result) 
                            (get meets-approval vote-result))
                       STATUS_PASSED
                       STATUS_REJECTED))
      )
      ;; Update proposal status
      (map-set proposals proposal-id
        (merge proposal {status: new-status})
      )
      
      (print {
        event: "proposal-finalized",
        proposal-id: proposal-id,
        status: new-status,
        vote-result: vote-result
      })
      
      (ok new-status)
    )
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
    )
    ;; Only admins can execute proposals
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    
    ;; Check if proposal has passed
    (asserts! (is-eq (get status proposal) STATUS_PASSED) ERR_PROPOSAL_NOT_PASSED)
    
    ;; Mark as executed
    (map-set proposals proposal-id
      (merge proposal {status: STATUS_EXECUTED})
    )
    
    (print {
      event: "proposal-executed",
      proposal-id: proposal-id,
      executor: tx-sender,
      proposal-type: (get proposal-type proposal)
    })
    
    ;; Note: Actual execution logic would depend on the specific proposal type
    ;; This could involve calling other contracts or updating parameters
    
    (ok true)
  )
)

;; Emergency functions (admin only)

(define-public (emergency-cancel-proposal (proposal-id uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (match (get-proposal proposal-id)
      proposal (begin
        (map-set proposals proposal-id
          (merge proposal {status: STATUS_REJECTED})
        )
        (print {
          event: "proposal-emergency-cancelled",
          proposal-id: proposal-id,
          admin: tx-sender
        })
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

;; Utility functions for batch operations

(define-public (batch-set-voter-weights (voters (list 10 {voter: principal, weight: uint})))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (ok (map set-single-voter-weight voters))
  )
)

(define-private (set-single-voter-weight (voter-data {voter: principal, weight: uint}))
  (map-set voter-weights (get voter voter-data) (get weight voter-data))
)

;; Events and logging
(define-public (get-proposal-history (start-id uint) (end-id uint))
  (ok (map get-proposal-safe (list start-id (+ start-id u1) (+ start-id u2) (+ start-id u3) (+ start-id u4))))
)

(define-private (get-proposal-safe (proposal-id uint))
  (default-to 
    {
      id: u0,
      proposer: CONTRACT_OWNER,
      proposal-type: u0,
      title: "",
      description: "",
      target-contract: none,
      function-name: none,
      parameters: none,
      votes-for: u0,
      votes-against: u0,
      status: u0,
      created-at: u0,
      voting-ends-at: u0
    }
    (map-get? proposals proposal-id)
  )
)