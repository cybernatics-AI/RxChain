;; rx-codebase.clar

;; Define the traits locally instead of importing them
(define-trait auth-trait
    (
        (verify-authorization (principal uint) (response bool uint))
    ))

(define-trait quality-trait
    (
        (record-quality-reading (uint int int int int) (response bool uint))
    ))

(define-trait batch-tracker-trait
    (
        (recall-batch (uint) (response bool uint))
    ))

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DATES (err u200))
(define-constant ERR-BATCH-NOT-FOUND (err u201))
(define-constant ERR-UNAUTHORIZED-TRANSFER (err u202))
(define-constant ERR-EXPIRED-BATCH (err u203))
(define-constant ERR-BATCH-NOT-APPROVED (err u204))
(define-constant ERR-INVALID-QUANTITY (err u205))
(define-constant ERR-INSUFFICIENT-QUANTITY (err u206))
(define-constant ERR-INVALID-STATUS-TRANSITION (err u207))
(define-constant ERR-ALREADY-RECALLED (err u208))
(define-constant ERR-INVALID-TEMPERATURE (err u209))
(define-constant ERR-INVALID-HUMIDITY (err u210))
(define-constant ERR-TRANSFER-LIST-FULL (err u211))
(define-constant ERR-INVALID-INPUT (err u212))

;; Constants for roles
(define-constant MANUFACTURER u1)
(define-constant DISTRIBUTOR u2)
(define-constant PHARMACY u3)
(define-constant REGULATOR u4)
(define-constant QUALITY_INSPECTOR u5)

;; Constants for batch status
(define-constant STATUS-CREATED u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-IN-TRANSIT u3)
(define-constant STATUS-DELIVERED u4)
(define-constant STATUS-DISPENSED u5)
(define-constant STATUS-RECALLED u6)
(define-constant STATUS-DESTROYED u7)

;; Quality thresholds
(define-constant MIN-TEMPERATURE -5)  ;; Celsius
(define-constant MAX-TEMPERATURE 25)  ;; Celsius
(define-constant MIN-HUMIDITY 20)     ;; Percentage
(define-constant MAX-HUMIDITY 60)     ;; Percentage

;; Enhanced drug batch struct with additional fields
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
        quality-metrics: (list 200 {
            timestamp: uint,
            temperature: int,
            humidity: int,
            location: {latitude: int, longitude: int},
            handler: principal
        }),
        is-approved: bool,
        recall-reason: (optional (string-ascii 256)),
        last-inspection-date: uint,
        storage-conditions: {
            required-temp-min: int,
            required-temp-max: int,
            required-humidity-min: int,
            required-humidity-max: int
        }
    })

;; Track batch transfers
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

;; Helper function to validate storage conditions
(define-private (validate-storage-conditions (conditions {
    required-temp-min: int,
    required-temp-max: int,
    required-humidity-min: int,
    required-humidity-max: int
}))
    (and
        (>= (get required-temp-min conditions) MIN-TEMPERATURE)
        (<= (get required-temp-max conditions) MAX-TEMPERATURE)
        (>= (get required-humidity-min conditions) MIN-HUMIDITY)
        (<= (get required-humidity-max conditions) MAX-HUMIDITY)
    ))

;; Register new batch with enhanced information
(define-public (register-batch 
    (auth-contract <auth-trait>)
    (manufacture-date uint) 
    (expiry-date uint)
    (drug-name (string-ascii 64))
    (batch-number (string-ascii 32))
    (quantity uint)
    (storage-conditions {
        required-temp-min: int,
        required-temp-max: int,
        required-humidity-min: int,
        required-humidity-max: int
    }))
    (let 
        (
            (batch-id (+ (var-get current-batch-id) u1))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender MANUFACTURER)))
            (current-time u0)
        )
        (begin
            (asserts! (> expiry-date manufacture-date) ERR-INVALID-DATES)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (> quantity u0) ERR-INVALID-QUANTITY)
            (asserts! (validate-storage-conditions storage-conditions) ERR-INVALID-INPUT)
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
                    remaining-quantity: quantity,
                    quality-metrics: (list),
                    is-approved: false,
                    recall-reason: none,
                    last-inspection-date: current-time,
                    storage-conditions: storage-conditions
                })
            (var-set current-batch-id batch-id)
            (ok batch-id))))

;; Enhanced transfer batch with partial quantities
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
            (asserts! (< current-time (get expiry-date (unwrap-panic batch))) ERR-EXPIRED-BATCH)
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
            
            (print {event: "transfer", batch-id: batch-id, new-owner: new-owner, quantity: transfer-quantity})
            (ok true))))

;; Dispense medication to patient
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

;; Enhanced quality reading with validation
(define-public (record-quality-reading 
    (auth-contract <auth-trait>)
    (quality-contract <quality-trait>)
    (batch-id uint)
    (temperature int)
    (humidity int)
    (latitude int)
    (longitude int))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender QUALITY_INSPECTOR)))
            (current-time u0)
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            
            ;; Validate temperature and humidity against batch requirements
            (asserts! (and 
                (>= temperature (get required-temp-min (get storage-conditions (unwrap-panic batch))))
                (<= temperature (get required-temp-max (get storage-conditions (unwrap-panic batch))))) 
                ERR-INVALID-TEMPERATURE)
            
            (asserts! (and
                (>= humidity (get required-humidity-min (get storage-conditions (unwrap-panic batch))))
                (<= humidity (get required-humidity-max (get storage-conditions (unwrap-panic batch)))))
                ERR-INVALID-HUMIDITY)
            
            ;; Update quality metrics
            (let ((new-metric {
                    timestamp: current-time,
                    temperature: temperature,
                    humidity: humidity,
                    location: {latitude: latitude, longitude: longitude},
                    handler: tx-sender
                })
                (current-metrics (get quality-metrics (unwrap-panic batch)))
                (updated-metrics (unwrap-panic (as-max-len? (append current-metrics new-metric) u200))))
                (map-set drug-batches batch-id
                    (merge (unwrap-panic batch)
                        {quality-metrics: updated-metrics})))
            
            (match (contract-call? quality-contract record-quality-reading 
                batch-id temperature humidity latitude longitude)
                success (ok success)
                error (err error)))))

;; Get dispensing history for a batch
(define-read-only (get-dispensing-history (batch-id uint))
    (ok (default-to (list) (map-get? dispensing-records batch-id))))

;; Get transfer history for a batch
(define-read-only (get-transfer-history (batch-id uint))
    (ok (default-to (list) (map-get? batch-transfers batch-id))))

;; Enhanced recall with reason
(define-public (recall-batch-with-reason
    (auth-contract <auth-trait>)
    (batch-tracker-contract <batch-tracker-trait>)
    (batch-id uint)
    (reason (string-ascii 256)))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender REGULATOR)))
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (not (is-eq (get current-status (unwrap-panic batch)) STATUS-RECALLED)) ERR-ALREADY-RECALLED)
            
            ;; Update batch status and add recall reason
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch)
                    {
                        current-status: STATUS-RECALLED,
                        recall-reason: (some reason)
                    }))
            
            (match (contract-call? batch-tracker-contract recall-batch batch-id)
                success (ok success)
                error (err error)))))

;; Mark batch as destroyed
(define-public (destroy-batch
    (auth-contract <auth-trait>)
    (batch-id uint))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization tx-sender REGULATOR)))
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get current-status (unwrap-panic batch)) STATUS-RECALLED) ERR-INVALID-STATUS-TRANSITION)
            
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch)
                    {current-status: STATUS-DESTROYED}))
            (ok true))))

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND)))

;; Get current batch count
(define-read-only (get-current-batch-count)
    (ok (var-get current-batch-id)))

;; Check if batch is recalled
(define-read-only (is-batch-recalled (batch-id uint))
    (match (map-get? drug-batches batch-id)
        batch (ok (is-eq (get current-status batch) STATUS-RECALLED))
        ERR-BATCH-NOT-FOUND))

;; Get batch status
(define-read-only (get-batch-status (batch-id uint))
    (ok (get current-status (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND))))

;; Transfer ownership of entire batch
(define-public (transfer-batch
    (auth-contract <auth-trait>)
    (batch-id uint)
    (new-owner principal))
    (let 
        (
            (batch (map-get? drug-batches batch-id))
            (auth-result (try! (contract-call? auth-contract verify-authorization new-owner DISTRIBUTOR)))
            (current-time u0)
        )
        (begin
            (asserts! (is-some batch) ERR-BATCH-NOT-FOUND)
            (asserts! (is-eq (get current-owner (unwrap-panic batch)) tx-sender) ERR-UNAUTHORIZED-TRANSFER)
            (asserts! (get is-approved (unwrap-panic batch)) ERR-BATCH-NOT-APPROVED)
            (asserts! (<= current-time (get expiry-date (unwrap-panic batch))) ERR-EXPIRED-BATCH)
            (asserts! auth-result ERR-NOT-AUTHORIZED)
            
            (map-set drug-batches batch-id
                (merge (unwrap-panic batch) 
                    {current-owner: new-owner}))
            
            (print {event: "transfer", batch-id: batch-id, new-owner: new-owner})
            (ok true))))
            