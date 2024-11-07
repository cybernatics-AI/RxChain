;; Adds quality metrics tracking and storage condition validation

;; Constants for roles
(define-constant MANUFACTURER u1)
(define-constant DISTRIBUTOR u2)
(define-constant PHARMACY u3)
(define-constant QUALITY_INSPECTOR u5)

;; Quality thresholds
(define-constant MIN-TEMPERATURE -5)  ;; Celsius
(define-constant MAX-TEMPERATURE 25)  ;; Celsius
(define-constant MIN-HUMIDITY 20)     ;; Percentage
(define-constant MAX-HUMIDITY 60)     ;; Percentage

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DATES (err u200))
(define-constant ERR-BATCH-NOT-FOUND (err u201))
(define-constant ERR-UNAUTHORIZED-TRANSFER (err u202))
(define-constant ERR-EXPIRED-BATCH (err u203))
(define-constant ERR-BATCH-NOT-APPROVED (err u204))
(define-constant ERR-INVALID-QUANTITY (err u205))
(define-constant ERR-INVALID-TEMPERATURE (err u209))
(define-constant ERR-INVALID-HUMIDITY (err u210))
(define-constant ERR-INVALID-INPUT (err u212))

;; Define traits
(define-trait auth-trait
    ((verify-authorization (principal uint) (response bool uint))))

(define-trait quality-trait
    ((record-quality-reading (uint int int int int) (response bool uint))))

;; Enhanced drug batch with quality metrics
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
        is-approved: bool,
        quality-metrics: (list 200 {
            timestamp: uint,
            temperature: int,
            humidity: int,
            location: {latitude: int, longitude: int},
            handler: principal
        }),
        storage-conditions: {
            required-temp-min: int,
            required-temp-max: int,
            required-humidity-min: int,
            required-humidity-max: int
        }
    })

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

;; Register batch with storage conditions
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
                    current-status: u1,
                    drug-name: drug-name,
                    batch-number: batch-number,
                    quantity: quantity,
                    is-approved: false,
                    quality-metrics: (list),
                    storage-conditions: storage-conditions
                })
            (var-set current-batch-id batch-id)
            (ok batch-id))))

;; Record quality measurements
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
            
            ;; Validate against batch requirements
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

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND)))

;; Get quality metrics for a batch
(define-read-only (get-quality-metrics (batch-id uint))
    (ok (get quality-metrics (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND))))

;; Get storage requirements for a batch
(define-read-only (get-storage-requirements (batch-id uint))
    (ok (get storage-conditions (unwrap! (map-get? drug-batches batch-id) ERR-BATCH-NOT-FOUND))))

;; Get current batch count
(define-read-only (get-current-batch-count)
    (ok (var-get current-batch-id)))