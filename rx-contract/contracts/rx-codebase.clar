;; Phase 1: Basic batch registration and tracking
;; Implements core functionality for registering drug batches

;; Constants for batch status
(define-constant STATUS-CREATED u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-RECALLED u6)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DATES (err u200))
(define-constant ERR-BATCH-NOT-FOUND (err u201))
(define-constant ERR-INVALID-QUANTITY (err u205))

;; Basic drug batch structure
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
        quantity: uint
    })

(define-data-var current-batch-id uint u0)

;; Register new batch
(define-public (register-batch 
    (manufacture-date uint) 
    (expiry-date uint)
    (drug-name (string-ascii 64))
    (batch-number (string-ascii 32))
    (quantity uint))
    (let 
        ((batch-id (+ (var-get current-batch-id) u1)))
        (begin
            (asserts! (> expiry-date manufacture-date) ERR-INVALID-DATES)
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
                    quantity: quantity
                })
            (var-set current-batch-id batch-id)
            (ok batch-id))))

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND)))

;; Get current batch count
(define-read-only (get-current-batch-count)
    (ok (var-get current-batch-id)))