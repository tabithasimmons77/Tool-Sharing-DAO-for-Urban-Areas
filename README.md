# 🔧 Tool-Sharing DAO for Urban Areas

A decentralized autonomous organization for community-owned tool sharing built on Stacks blockchain using Clarity smart contracts.

## 🚀 Features

- **Tool Registration**: Community members can register their tools for sharing
- **Secure Borrowing**: Deposit-based borrowing system with time limits
- **Rating System**: Rate tools and users to build trust
- **Automatic Penalties**: Late return penalties fund the DAO treasury
- **Emergency Returns**: Tool owners can reclaim tools if needed

## 🏗️ Contract Functions

### 📝 Tool Management

- `register-tool` - Register a new tool for sharing
- `get-tool` - Get tool information by ID
- `get-available-tools` - List all available tools

### 💰 Borrowing System

- `borrow-tool` - Borrow a tool with deposit payment
- `return-tool` - Return a borrowed tool and get deposit refund
- `get-loan` - Get loan information by ID
- `is-loan-overdue` - Check if a loan is overdue

### ⭐ Rating System

- `rate-tool` - Rate a tool (1-5 stars)
- `rate-user` - Rate a user (1-5 stars)
- `get-user-rating` - Get user's average rating

### 🚨 Emergency Functions

- `emergency-return` - Tool owner can force return (with penalty)

## 📋 Usage Instructions

### 1. Register a Tool 🛠️

```clarity
(contract-call? .Tool-Sharing-DAO-for-Urban register-tool 
  u"Power Drill" 
  u"Tools" 
  u1000000  ;; 1 STX deposit
  u100000)  ;; 0.1 STX daily rate
```

### 2. Borrow a Tool 📦

```clarity
(contract-call? .Tool-Sharing-DAO-for-Urban borrow-tool 
  u1        ;; tool ID
  u144)     ;; duration in blocks (1 day ≈ 144 blocks)
```

### 3. Return a Tool ✅

```clarity
(contract-call? .Tool-Sharing-DAO-for-Urban return-tool u1) ;; loan ID
```

### 4. Rate a Tool ⭐

```clarity
(contract-call? .Tool-Sharing-DAO-for-Urban rate-tool 
  u1  ;; tool ID
  u5) ;; rating (1-5)
```

## 🔧 Development Setup

1. **Install Clarinet**:
   ```bash
   npm install -g @hirosystems/clarinet-cli
   ```

2. **Run Tests**:
   ```bash
   clarinet test
   ```

3. **Deploy Contract**:
   ```bash
   clarinet deploy
   ```

## 💡 How It Works

1. **Tool Registration**: Users register tools with deposit requirements and daily rates
2. **Borrowing**: Users pay deposits to borrow tools for specified durations
3. **Returns**: Timely returns get full deposit refunds; late returns incur penalties
4. **Community Trust**: Rating system builds reputation for tools and users
5. **DAO Treasury**: Penalties fund community initiatives

## 🔒 Security Features

- Deposit-based borrowing ensures tool return incentives
- Owner emergency return for problematic loans
- Overdue penalty system protects tool owners
- Rating system builds community trust

## 📊 Contract Data

- **Tools**: ID, owner, name, category, deposit, rate, availability, rating
- **Loans**: ID, tool, borrower, duration, deposit, return status
- **User Ratings**: Aggregate ratings and counts
- **DAO Treasury**: Community fund from penalties

## 🎯 Future Enhancements

- Multi-sig governance for dispute resolution
- Token-based rewards for active participants
- Geographic filtering for local tool discovery
- Integration with reputation systems

---

Built with ❤️ on Stacks blockchain using Clarity smart contracts
