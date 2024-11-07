;; Adds partial quantity transfers and dispensing records

;; Constants for roles
(define-constant MANUFACTURER u1)
(define-constant DISTRIBUTOR u2)
(define-constant PHARMACY u3)
(define-constant QUALITY_INSPECTOR u5)

;; Constants for batch status
(define-constant STATUS-CREATED u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-IN-TRANSIT u3)
(define-constant STATUS-DELIVERED u4)
(define-constant STATUS-DISPENSED u5)
(define-constant STATUS-RECALLED u6)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BATCH-NOT-FOUND (err u201))
(define-constant ERR-UNAUTHORIZED-TRANSFER (err u202))
(define-constant ERR-EXPIRED-BATCH (err u203))
(define-constant ERR-BATCH-NOT-APPROVED (err u204))
(define-constant ERR-INVALID-QUANTITY (err u205))
(define-constant ERR-INSUFFICIENT-QUANTITY (err u206))
(define-constant ERR-ALREADY-RECALLED (err u208))
(define-constant ERR-TRANSFER-LIST-FULL (err u211))

;; Define auth trait
(define-trait auth-trait
    ((verify-authorization (principal uint) (response bool uint))))

;; Enhanced drug batch with remaining quantity
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
        remaining-quantity: uint,
        is-approved: bool
    })

;; Track batch transfers with quantities
(define-map batch-transfers 
    uint 
    (list 100 {
        from: principal,
        to: principal,
        timestamp: uint,
        quantity: uint
    }))

;; Track dispensing history
(define-map dispensing-records
    uint
    (list 1000 {
        pharmacy: principal,
        timestamp: uint,
        quantity: uint,
        prescription-id: (string-ascii 64)
    }))

(define-data-var current-batch-id uint u0)

;; Transfer partial quantity
(define-public (transfer-batch-quantity
    (auth-contract <auth-trait>)
    (batch-id uint) 
    (new-owner principal)
    (transfer-quantity uint))
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
            (asserts! (<= transfer-quantity (get remaining-quantity (unwrap-panic batch))) ERR-INSUFFICIENT-QUANTITY)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (< (len current-transfers) u100) ERR-TRANSFER-LIST-FULL)
            
            ;; Update remaining quantity
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch) 
                    {
                        remaining-quantity: (- (get remaining-quantity (unwrap-panic batch)) transfer-quantity)
                    }))
            
            ;; Record transfer
            (let ((new-transfer {
                    from: tx-sender,
                    to: new-owner,
                    timestamp: current-time,
                    quantity: transfer-quantity
                })
                (updated-transfers (unwrap-panic (as-max-len? (append current-transfers new-transfer) u100))))
                (map-set batch-transfers batch-id updated-transfers))
            
            (ok true))))

;; Dispense medication
(define-public (dispense-medication
    (auth-contract <auth-trait>)
    (batch-id uint)
    (quantity uint)
    (prescription-id (string-ascii 64)))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender PHARMACY)))
            (current-time u0)
            (current-records (default-to (list) (map-get? dispensing-records batch-id)))
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (<= quantity (get remaining-quantity (unwrap-panic batch))) ERR-INSUFFICIENT-QUANTITY)
            (asserts! (not (is-eq (get current-status (unwrap-panic batch)) STATUS-RECALLED)) ERR-ALREADY-RECALLED)
            (asserts! (< (len current-records) u1000) ERR-TRANSFER-LIST-FULL)
            
            ;; Update remaining quantity
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch)
                    {
                        remaining-quantity: (- (get remaining-quantity (unwrap-panic batch)) quantity),
                        current-status: STATUS-DISPENSED
                    }))
            
            ;; Record dispensing
            (let ((new-record {
                    pharmacy: tx-sender,
                    timestamp: current-time,
                    quantity: quantity,
                    prescription-id: prescription-id
                })
                (updated-records (unwrap-panic (as-max-len? (append current-records new-record) u1000))))
                (map-set dispensing-records batch-id updated-records))
            
            (ok true))))

;; Get dispensing history
(define-read-only (get-dispensing-history (batch-id uint))
    (ok (default-to (list) (map-get? dispensing-records batch-id))))

;; Get transfer history
(define-read-only (get-transfer-history (batch-id uint))
    (ok (default-to (list) (map-get? batch-transfers batch-id))))

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND)))