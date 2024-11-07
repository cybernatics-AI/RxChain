;; Builds on Phase 1 by adding transfer functionality and role-based authorization

;; Constants for roles
(define-constant MANUFACTURER u1)
(define-constant DISTRIBUTOR u2)
(define-constant PHARMACY u3)

;; Constants for batch status
(define-constant STATUS-CREATED u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-IN-TRANSIT u3)
(define-constant STATUS-DELIVERED u4)
(define-constant STATUS-RECALLED u6)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DATES (err u200))
(define-constant ERR-BATCH-NOT-FOUND (err u201))
(define-constant ERR-UNAUTHORIZED-TRANSFER (err u202))
(define-constant ERR-EXPIRED-BATCH (err u203))
(define-constant ERR-BATCH-NOT-APPROVED (err u204))
(define-constant ERR-INVALID-QUANTITY (err u205))

;; Define auth trait
(define-trait auth-trait
    ((verify-authorization (principal uint) (response bool uint))))

;; Enhanced drug batch structure with transfer tracking
(define-map drug-batches
    uint
    {
        manufacturer-id: principal,
        manufacture-date: uint,
        expiry-date: uint,
        current-owner: principal,
        current-status: uint,
        drug-name: (string-ascii 64),
        batch-number: (string-ascii 32),
        quantity: uint,
        is-approved: bool
    })

;; Track batch transfers
(define-map batch-transfers 
    uint 
    (list 100 {
        from: principal,
        to: principal,
        timestamp: uint
    }))

(define-data-var current-batch-id uint u0)

;; Register new batch
(define-public (register-batch 
    (auth-contract <auth-trait>)
    (manufacture-date uint) 
    (expiry-date uint)
    (drug-name (string-ascii 64))
    (batch-number (string-ascii 32))
    (quantity uint))
    (let 
        (
            (batch-id (+ (var-get current-batch-id) u1))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender MANUFACTURER)))
        )
        (begin
            (asserts! (> expiry-date manufacture-date) ERR-INVALID-DATES)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (> quantity u0) ERR-INVALID-QUANTITY)
            
            (map-set drug-batches batch-id
                {
                    manufacturer-id: tx-sender,
                    manufacture-date: manufacture-date,
                    expiry-date: expiry-date,
                    current-owner: tx-sender,
                    current-status: STATUS-CREATED,
                    drug-name: drug-name,
                    batch-number: batch-number,
                    quantity: quantity,
                    is-approved: false
                })
            (var-set current-batch-id batch-id)
            (ok batch-id))))

;; Transfer batch ownership
(define-public (transfer-batch
    (auth-contract <auth-trait>)
    (batch-id uint)
    (new-owner principal))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization new-owner DISTRIBUTOR)))
            (current-time u0)
            (current-transfers (default-to (list) (map-get? batch-transfers batch-id)))
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! (is-eq (get current-owner (unwrap-panic batch)) tx-sender) ERR-UNAUTHORIZED-TRANSFER)
            (asserts! (get is-approved (unwrap-panic batch)) ERR-BATCH-NOT-APPROVED)
            (asserts! (<= current-time (get expiry-date (unwrap-panic batch))) ERR-EXPIRED-BATCH)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            
            ;; Update ownership
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch) 
                    {current-owner: new-owner}))
            
            ;; Record transfer
            (let ((new-transfer {
                    from: tx-sender,
                    to: new-owner,
                    timestamp: current-time
                })
                (updated-transfers (unwrap-panic (as-max-len? (append current-transfers new-transfer) u100))))
                (map-set batch-transfers batch-id updated-transfers))
            
            (ok true))))

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND)))

;; Get transfer history
(define-read-only (get-transfer-history (batch-id uint))
    (ok (default-to (list) (map-get? batch-transfers batch-id))))

;; Get current batch count
(define-read-only (get-current-batch-count)
    (ok (var-get current-batch-id)))