# 🛡️ Insurance Payout Contract

A smart contract implementation for managing insurance policies and claims with conditional payouts on the Stacks blockchain.

## 📋 Overview

This contract demonstrates conditional payment logic through an insurance system where:
- 👥 Users can purchase insurance policies by paying premiums
- 📝 Policyholders can submit claims against their active policies  
- ✅ Contract owner reviews and approves/rejects claims
- 💰 Approved claims trigger automatic payouts to claimants

## 🚀 Features

- **Policy Management**: Create and cancel insurance policies
- **Claims Processing**: Submit, approve, reject, and payout claims
- **Conditional Payments**: Payouts only execute when specific conditions are met
- **Premium Calculation**: Automatic premium calculation (10% of coverage)
- **Balance Tracking**: Contract maintains internal balance for payouts
- **Time-based Expiry**: Policies expire after specified block duration

## 📖 Usage Instructions

### Creating a Policy

```clarity
(contract-call? .insurance-payout-contract create-policy u10000 u1000)
```
- Creates policy with 10,000 STX coverage for 1,000 blocks
- Premium automatically calculated as 1,000 STX (10% of coverage)

### Submitting a Claim

```clarity
(contract-call? .insurance-payout-contract submit-claim u1 u5000 "Car accident damage")
```
- Submit claim against policy #1 for 5,000 STX
- Must be policy holder and policy must be active

### Approving Claims (Owner Only)

```clarity
(contract-call? .insurance-payout-contract approve-claim u1)
```

### Processing Payouts

```clarity
(contract-call? .insurance-payout-contract process-payout u1)
```
- Transfers approved claim amount to claimant
- Deactivates the policy after payout

### Checking Policy Status

```clarity
(contract-call? .insurance-payout-contract get-policy u1)
(contract-call? .insurance-payout-contract is-policy-active u1)
```

## 🔧 Contract Functions

### Public Functions
- `create-policy` - Purchase new insurance policy
- `submit-claim` - File claim against active policy
- `approve-claim` - Approve pending claim (owner only)
- `reject-claim` - Reject pending claim (owner only)
- `process-payout` - Execute payout for approved claim
- `cancel-policy` - Cancel active policy with 50% refund
- `add-funds` - Add funds to contract balance (owner only)

### Read-Only Functions
- `get-policy` - Retrieve policy details
- `get-claim` - Retrieve claim details
- `get-contract-balance` - Check contract STX balance
- `is-policy-active` - Check if policy is active and not expired
- `calculate-premium` - Calculate premium for coverage amount

## 🎯 Learning Objectives

This contract teaches:
- ✨ **Conditional Logic**: Payments only execute when conditions are met
- 🔐 **Access Control**: Owner-only functions for claim approval
- ⏰ **Time-based Logic**: Policy expiration using block heights
- 💾 **State Management**: Tracking policies, claims, and balances
- 🔄 **Multi-step Workflows**: Claim submission → approval → payout flow

## 🛠️ Development

Deploy with Clarinet:

```bash
clarinet deploy
```

Run tests:

```bash
clarinet test
```

## 📊 Contract States

**Policy Status**: Active/Inactive + expiration check
**Claim Status**: pending → approved/rejected → paid
**Balance Tracking**: Automatic STX balance management

## ⚠️ Important Notes

- Premiums are set at 10% of coverage amount
- Policies expire based on block height
- Claims must be submitted before policy expiration
- Only one claim per policy (policy deactivates after payout)
- Policy cancellation provides 50% premium refund
```

**Git Commit Message:**
```
feat: implement insurance payout contract with conditional payments
```

**GitHub Pull Request Title:**
```
Add Insurance Payout Contract MVP with Conditional Payment Logic
```

**GitHub Pull Request Description:**
```
## Summary
Implements a complete insurance payout contract demonstrating conditional payment patterns in Clarity.

## Features Added
- Policy creation and management system
- Claims submission and approval workflow  
- Conditional payout execution based on claim status
- Premium calculation and balance tracking
- Time-based policy expiration logic
- Owner controls for claim approval/rejection

## Technical Implementation
- Uses maps for policy and claim storage
- Implements multi-step conditional workflows
- Includes proper error handling and access controls
- Demonstrates STX transfers with contract-as-principal pattern

## Learning Focus
This contract teaches conditional payment logic where payouts only execute when specific conditions are met (approved claims, sufficient balance, active policies, etc.).

