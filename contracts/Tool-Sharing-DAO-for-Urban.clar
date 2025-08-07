(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-TOOL-NOT-FOUND (err u101))
(define-constant ERR-TOOL-UNAVAILABLE (err u102))
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u103))
(define-constant ERR-INVALID-DURATION (err u104))
(define-constant ERR-LOAN-EXPIRED (err u105))
(define-constant ERR-LOAN-ACTIVE (err u106))
(define-constant ERR-INVALID-RATING (err u107))

(define-constant ERR-MAINTENANCE-REQUIRED (err u108))
(define-constant ERR-NOT-MAINTENANCE-PROVIDER (err u109))
(define-constant WEAR-PER-USE u10)
(define-constant MAINTENANCE-THRESHOLD u80)

(define-map tools uint {
  owner: principal,
  name: (string-utf8 64), 
  category: (string-utf8 32),
  deposit-required: uint,
  daily-rate: uint,
  available: bool,
  total-loans: uint,
  rating: uint
})

(define-map loans uint {
  tool-id: uint,
  borrower: principal,
  start-block: uint,
  duration-blocks: uint,
  deposit-paid: uint,
  returned: bool
})

(define-map user-ratings principal {
  total-rating: uint,
  rating-count: uint
})

(define-data-var next-tool-id uint u1)
(define-data-var next-loan-id uint u1)
(define-data-var dao-treasury uint u0)

(define-public (register-tool (name (string-utf8 64)) (category (string-utf8 32)) (deposit-required uint) (daily-rate uint))
  (let ((tool-id (var-get next-tool-id)))
    (map-set tools tool-id {
      owner: tx-sender,
      name: name,
      category: category,
      deposit-required: deposit-required,
      daily-rate: daily-rate,
      available: true,
      total-loans: u0,
      rating: u5
    })
    (var-set next-tool-id (+ tool-id u1))
    (ok tool-id)))

(define-public (borrow-tool (tool-id uint) (duration-blocks uint))
  (let ((tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND))
        (loan-id (var-get next-loan-id))
        (deposit (get deposit-required tool))
        (rate (get daily-rate tool)))
    (asserts! (get available tool) ERR-TOOL-UNAVAILABLE)
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    (asserts! (>= (stx-get-balance tx-sender) deposit) ERR-INSUFFICIENT-DEPOSIT)
    
    (try! (stx-transfer? deposit tx-sender (as-contract tx-sender)))
    
    (map-set loans loan-id {
      tool-id: tool-id,
      borrower: tx-sender,
      start-block: stacks-block-height,
      duration-blocks: duration-blocks,
      deposit-paid: deposit,
      returned: false
    })
    
    (map-set tools tool-id (merge tool {available: false}))
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)))

(define-public (return-tool (loan-id uint))
  (let ((loan (unwrap! (map-get? loans loan-id) ERR-TOOL-NOT-FOUND))
        (tool-id (get tool-id loan))
        (tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND)))
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (get returned loan)) ERR-LOAN-ACTIVE)
    
    (map-set loans loan-id (merge loan {returned: true}))
    (map-set tools tool-id (merge tool {
      available: true,
      total-loans: (+ (get total-loans tool) u1)
    }))
    
    (let ((deposit (get deposit-paid loan))
          (overdue-blocks (if (> (+ (get start-block loan) (get duration-blocks loan)) stacks-block-height)
                             u0
                             (- stacks-block-height (+ (get start-block loan) (get duration-blocks loan)))))
          (penalty (if (> overdue-blocks u0) (/ deposit u10) u0))
          (refund (- deposit penalty)))
      (if (> penalty u0)
        (var-set dao-treasury (+ (var-get dao-treasury) penalty))
        true)
      (try! (as-contract (stx-transfer? refund tx-sender (get borrower loan))))
      (ok refund))))

(define-public (rate-tool (tool-id uint) (rating uint))
  (let ((tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND)))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (let ((current-rating (get rating tool))
          (loan-count (get total-loans tool)))
      (if (> loan-count u0)
        (let ((new-rating (/ (+ (* current-rating loan-count) rating) (+ loan-count u1))))
          (map-set tools tool-id (merge tool {rating: new-rating}))
          (ok new-rating))
        (ok current-rating)))))

(define-public (rate-user (user principal) (rating uint))
  (let ((user-rating (default-to {total-rating: u0, rating-count: u0} (map-get? user-ratings user))))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (let ((new-total (+ (get total-rating user-rating) rating))
          (new-count (+ (get rating-count user-rating) u1)))
      (map-set user-ratings user {
        total-rating: new-total,
        rating-count: new-count
      })
      (ok (/ new-total new-count)))))

(define-public (emergency-return (loan-id uint))
  (let ((loan (unwrap! (map-get? loans loan-id) ERR-TOOL-NOT-FOUND))
        (tool-id (get tool-id loan))
        (tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND)))
    (asserts! (is-eq (get owner tool) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (get returned loan)) ERR-LOAN-ACTIVE)
    
    (map-set loans loan-id (merge loan {returned: true}))
    (map-set tools tool-id (merge tool {available: true}))
    
    (let ((deposit (get deposit-paid loan))
          (penalty (/ deposit u5)))
      (var-set dao-treasury (+ (var-get dao-treasury) penalty))
      (try! (as-contract (stx-transfer? (- deposit penalty) tx-sender (get borrower loan))))
      (ok true))))

(define-read-only (get-tool (tool-id uint))
  (map-get? tools tool-id))

(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id))

(define-read-only (get-user-rating (user principal))
  (map-get? user-ratings user))

(define-read-only (get-dao-treasury)
  (var-get dao-treasury))

(define-read-only (is-loan-overdue (loan-id uint))
  (match (map-get? loans loan-id)
    loan (let ((end-block (+ (get start-block loan) (get duration-blocks loan))))
           (and (not (get returned loan)) (> stacks-block-height end-block)))
    false))

(define-read-only (get-available-tools)
  (let ((total-tools (var-get next-tool-id)))
    (filter is-available (map get-tool-with-id (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))))

(define-private (is-available (tool-opt (optional {owner: principal, name: (string-utf8 64), category: (string-utf8 32), deposit-required: uint, daily-rate: uint, available: bool, total-loans: uint, rating: uint})))
  (match tool-opt
    tool (get available tool)
    false))

(define-private (get-tool-with-id (tool-id uint))
  (map-get? tools tool-id))

(define-map tool-maintenance uint {
  wear-level: uint,
  last-maintenance-block: uint,
  maintenance-count: uint,
  maintenance-due: bool
})

(define-map maintenance-providers principal {
  registered: bool,
  completed-jobs: uint,
  provider-rating: uint
})

(define-map maintenance-requests uint {
  tool-id: uint,
  provider: principal,
  requested-block: uint,
  completed: bool,
  cost: uint
})

(define-data-var next-maintenance-id uint u1)

(define-public (register-maintenance-provider)
  (begin
    (map-set maintenance-providers tx-sender {
      registered: true,
      completed-jobs: u0,
      provider-rating: u5
    })
    (ok true)))

(define-public (schedule-maintenance (tool-id uint) (provider principal) (cost uint))
  (let ((tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND))
        (maintenance-info (default-to {wear-level: u0, last-maintenance-block: u0, maintenance-count: u0, maintenance-due: false} 
                                     (map-get? tool-maintenance tool-id)))
        (provider-info (unwrap! (map-get? maintenance-providers provider) ERR-NOT-MAINTENANCE-PROVIDER))
        (request-id (var-get next-maintenance-id)))
    (asserts! (is-eq (get owner tool) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (get registered provider-info) ERR-NOT-MAINTENANCE-PROVIDER)
    
    (map-set maintenance-requests request-id {
      tool-id: tool-id,
      provider: provider,
      requested-block: stacks-block-height,
      completed: false,
      cost: cost
    })
    
    (var-set next-maintenance-id (+ request-id u1))
    (ok request-id)))

(define-public (complete-maintenance (request-id uint))
  (let ((request (unwrap! (map-get? maintenance-requests request-id) ERR-TOOL-NOT-FOUND))
        (tool-id (get tool-id request))
        (maintenance-info (default-to {wear-level: u0, last-maintenance-block: u0, maintenance-count: u0, maintenance-due: false} 
                                     (map-get? tool-maintenance tool-id))))
    (asserts! (is-eq (get provider request) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (get completed request)) ERR-LOAN-ACTIVE)
    
    (map-set maintenance-requests request-id (merge request {completed: true}))
    (map-set tool-maintenance tool-id {
      wear-level: u0,
      last-maintenance-block: stacks-block-height,
      maintenance-count: (+ (get maintenance-count maintenance-info) u1),
      maintenance-due: false
    })
    
    (let ((provider-info (unwrap! (map-get? maintenance-providers tx-sender) ERR-NOT-MAINTENANCE-PROVIDER)))
      (map-set maintenance-providers tx-sender 
        (merge provider-info {completed-jobs: (+ (get completed-jobs provider-info) u1)})))
    (ok true)))

(define-private (update-tool-wear (tool-id uint))
  (let ((maintenance-info (default-to {wear-level: u0, last-maintenance-block: u0, maintenance-count: u0, maintenance-due: false} 
                                     (map-get? tool-maintenance tool-id)))
        (new-wear (+ (get wear-level maintenance-info) WEAR-PER-USE)))
    (map-set tool-maintenance tool-id (merge maintenance-info {
      wear-level: new-wear,
      maintenance-due: (>= new-wear MAINTENANCE-THRESHOLD)
    }))
    new-wear))

(define-read-only (get-tool-condition (tool-id uint))
  (map-get? tool-maintenance tool-id))

(define-read-only (needs-maintenance (tool-id uint))
  (match (map-get? tool-maintenance tool-id)
    maintenance-info (get maintenance-due maintenance-info)
    false))