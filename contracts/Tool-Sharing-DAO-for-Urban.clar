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

(define-constant POINTS-TOOL-REGISTER u50)
(define-constant POINTS-SUCCESSFUL-LOAN u25)
(define-constant POINTS-MAINTENANCE-COMPLETE u100)
(define-constant POINTS-HIGH-RATING u75)
(define-constant REWARD-RATE u1000)

(define-constant BADGE-FIRST-TOOL "first-tool")
(define-constant BADGE-SUPER-BORROWER "super-borrower") 
(define-constant BADGE-TOOL-MASTER "tool-master")
(define-constant BADGE-MAINTENANCE-PRO "maintenance-pro")

(define-constant ERR-INVALID-SUBSCRIPTION (err u110))
(define-constant ERR-SUBSCRIPTION-EXPIRED (err u111))

(define-constant SUBSCRIPTION-WEEKLY-BLOCKS u1008)
(define-constant SUBSCRIPTION-MONTHLY-BLOCKS u4320)
(define-constant SUBSCRIPTION-QUARTERLY-BLOCKS u12960)

(define-constant SUBSCRIPTION-WEEKLY-COST u5000000)
(define-constant SUBSCRIPTION-MONTHLY-COST u15000000)
(define-constant SUBSCRIPTION-QUARTERLY-COST u40000000)

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


(define-map user-rewards principal {
  total-points: uint,
  claimed-rewards: uint,
  badges: (list 10 (string-ascii 20)),
  tools-registered: uint,
  successful-loans: uint,
  maintenance-completed: uint
})

(define-public (award-points (user principal) (points uint) (activity (string-ascii 20)))
  (let ((current-rewards (default-to {total-points: u0, claimed-rewards: u0, badges: (list), tools-registered: u0, successful-loans: u0, maintenance-completed: u0} 
                                    (map-get? user-rewards user))))
    (map-set user-rewards user (merge current-rewards {total-points: (+ (get total-points current-rewards) points)}))
    (check-and-award-badges user activity)
    (ok points)))

(define-private (check-and-award-badges (user principal) (activity (string-ascii 20)))
  (let ((rewards (default-to {total-points: u0, claimed-rewards: u0, badges: (list), tools-registered: u0, successful-loans: u0, maintenance-completed: u0} (map-get? user-rewards user))))
    (if (is-eq activity "tool-register")
      (begin
        (map-set user-rewards user (merge rewards {tools-registered: (+ (get tools-registered rewards) u1)}))
        (if (is-eq (get tools-registered rewards) u0)
          (award-badge user BADGE-FIRST-TOOL)
          true))
      true)
    (if (is-eq activity "loan-complete")
      (begin
        (map-set user-rewards user (merge rewards {successful-loans: (+ (get successful-loans rewards) u1)}))
        (if (is-eq (get successful-loans rewards) u9)
          (award-badge user BADGE-SUPER-BORROWER)
          true))
      true)
    (if (is-eq (get tools-registered rewards) u4)
      (award-badge user BADGE-TOOL-MASTER)
      true)
    true))

(define-private (award-badge (user principal) (badge (string-ascii 20)))
  (let ((rewards (default-to {total-points: u0, claimed-rewards: u0, badges: (list), tools-registered: u0, successful-loans: u0, maintenance-completed: u0} (map-get? user-rewards user)))
        (new-badges (match (as-max-len? (append (get badges rewards) badge) u10)
                      some-badges some-badges
                      (get badges rewards))))
    (map-set user-rewards user (merge rewards {badges: new-badges}))
    true))

(define-public (claim-rewards)
  (let ((rewards (unwrap! (map-get? user-rewards tx-sender) ERR-UNAUTHORIZED))
        (available-points (- (get total-points rewards) (get claimed-rewards rewards)))
        (stx-amount (/ available-points REWARD-RATE)))
    (asserts! (> stx-amount u0) ERR-INSUFFICIENT-DEPOSIT)
    (asserts! (>= (var-get dao-treasury) stx-amount) ERR-INSUFFICIENT-DEPOSIT)
    
    (var-set dao-treasury (- (var-get dao-treasury) stx-amount))
    (map-set user-rewards tx-sender (merge rewards {claimed-rewards: (get total-points rewards)}))
    (try! (as-contract (stx-transfer? stx-amount tx-sender tx-sender)))
    (ok stx-amount)))

(define-read-only (get-user-rewards (user principal))
  (map-get? user-rewards user))

(define-read-only (get-available-rewards (user principal))
  (match (map-get? user-rewards user)
    rewards (/ (- (get total-points rewards) (get claimed-rewards rewards)) REWARD-RATE)
    u0))


(define-map user-subscriptions principal {
  active: bool,
  tier: (string-ascii 20),
  purchase-block: uint,
  expiry-block: uint,
  total-borrows: uint
})

(define-public (purchase-subscription (tier (string-ascii 20)))
  (let ((cost (if (is-eq tier "weekly")
                 SUBSCRIPTION-WEEKLY-COST
                 (if (is-eq tier "monthly")
                    SUBSCRIPTION-MONTHLY-COST
                    (if (is-eq tier "quarterly")
                       SUBSCRIPTION-QUARTERLY-COST
                       u0))))
        (duration (if (is-eq tier "weekly")
                     SUBSCRIPTION-WEEKLY-BLOCKS
                     (if (is-eq tier "monthly")
                        SUBSCRIPTION-MONTHLY-BLOCKS
                        (if (is-eq tier "quarterly")
                           SUBSCRIPTION-QUARTERLY-BLOCKS
                           u0)))))
    (asserts! (> cost u0) ERR-INVALID-SUBSCRIPTION)
    (asserts! (>= (stx-get-balance tx-sender) cost) ERR-INSUFFICIENT-DEPOSIT)
    
    (try! (stx-transfer? cost tx-sender (as-contract tx-sender)))
    (var-set dao-treasury (+ (var-get dao-treasury) cost))
    
    (map-set user-subscriptions tx-sender {
      active: true,
      tier: tier,
      purchase-block: stacks-block-height,
      expiry-block: (+ stacks-block-height duration),
      total-borrows: u0
    })
    (ok true)))

(define-public (borrow-with-subscription (tool-id uint) (duration-blocks uint))
  (let ((subscription (unwrap! (map-get? user-subscriptions tx-sender) ERR-INVALID-SUBSCRIPTION))
        (tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND))
        (loan-id (var-get next-loan-id)))
    (asserts! (get active subscription) ERR-INVALID-SUBSCRIPTION)
    (asserts! (> (get expiry-block subscription) stacks-block-height) ERR-SUBSCRIPTION-EXPIRED)
    (asserts! (get available tool) ERR-TOOL-UNAVAILABLE)
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    
    (map-set loans loan-id {
      tool-id: tool-id,
      borrower: tx-sender,
      start-block: stacks-block-height,
      duration-blocks: duration-blocks,
      deposit-paid: u0,
      returned: false
    })
    
    (map-set tools tool-id (merge tool {available: false}))
    (map-set user-subscriptions tx-sender 
      (merge subscription {total-borrows: (+ (get total-borrows subscription) u1)}))
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)))

(define-read-only (get-subscription-status (user principal))
  (match (map-get? user-subscriptions user)
    subscription (ok {
      active: (and (get active subscription) 
                  (> (get expiry-block subscription) stacks-block-height)),
      tier: (get tier subscription),
      blocks-remaining: (if (> (get expiry-block subscription) stacks-block-height)
                           (- (get expiry-block subscription) stacks-block-height)
                           u0),
      total-borrows: (get total-borrows subscription)
    })
    ERR-INVALID-SUBSCRIPTION))


    (define-constant ERR-CLAIM-NOT-FOUND (err u112))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u113))
(define-constant ERR-INSUFFICIENT-POOL (err u114))
(define-constant INSURANCE-FEE-PERCENTAGE u5)

(define-data-var insurance-pool-balance uint u0)
(define-data-var next-claim-id uint u1)

(define-map insurance-claims uint {
  loan-id: uint,
  tool-id: uint,
  claimant: principal,
  claim-amount: uint,
  claim-block: uint,
  approved: bool,
  processed: bool,
  claim-reason: (string-ascii 50)
})

(define-map insured-loans uint {
  insured: bool,
  insurance-paid: uint
})

(define-public (borrow-tool-insured (tool-id uint) (duration-blocks uint))
  (let ((tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND))
        (loan-id (var-get next-loan-id))
        (deposit (get deposit-required tool))
        (insurance-fee (/ (* deposit INSURANCE-FEE-PERCENTAGE) u100)))
    (asserts! (get available tool) ERR-TOOL-UNAVAILABLE)
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    (asserts! (>= (stx-get-balance tx-sender) (+ deposit insurance-fee)) ERR-INSUFFICIENT-DEPOSIT)
    
    (try! (stx-transfer? (+ deposit insurance-fee) tx-sender (as-contract tx-sender)))
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) insurance-fee))
    
    (map-set loans loan-id {
      tool-id: tool-id,
      borrower: tx-sender,
      start-block: stacks-block-height,
      duration-blocks: duration-blocks,
      deposit-paid: deposit,
      returned: false
    })
    
    (map-set insured-loans loan-id {insured: true, insurance-paid: insurance-fee})
    (map-set tools tool-id (merge tool {available: false}))
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)))

(define-public (file-insurance-claim (loan-id uint) (claim-amount uint) (reason (string-ascii 50)))
  (let ((loan (unwrap! (map-get? loans loan-id) ERR-TOOL-NOT-FOUND))
        (tool-id (get tool-id loan))
        (tool (unwrap! (map-get? tools tool-id) ERR-TOOL-NOT-FOUND))
        (claim-id (var-get next-claim-id)))
    (asserts! (is-eq (get owner tool) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (get returned loan) ERR-LOAN-ACTIVE)
    
    (map-set insurance-claims claim-id {
      loan-id: loan-id,
      tool-id: tool-id,
      claimant: tx-sender,
      claim-amount: claim-amount,
      claim-block: stacks-block-height,
      approved: false,
      processed: false,
      claim-reason: reason
    })
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)))

(define-public (process-claim (claim-id uint) (approved bool))
  (let ((claim (unwrap! (map-get? insurance-claims claim-id) ERR-CLAIM-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (not (get processed claim)) ERR-CLAIM-ALREADY-PROCESSED)
    
    (if approved
      (let ((claim-amount (get claim-amount claim))
            (pool-balance (var-get insurance-pool-balance)))
        (asserts! (>= pool-balance claim-amount) ERR-INSUFFICIENT-POOL)
        (var-set insurance-pool-balance (- pool-balance claim-amount))
        (try! (as-contract (stx-transfer? claim-amount tx-sender (get claimant claim))))
        (map-set insurance-claims claim-id (merge claim {approved: true, processed: true}))
        (ok claim-amount))
      (begin
        (map-set insurance-claims claim-id (merge claim {processed: true}))
        (ok u0)))))

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool-balance))

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims claim-id))

(define-read-only (is-loan-insured (loan-id uint))
  (match (map-get? insured-loans loan-id)
    insurance-info (get insured insurance-info)
    false))