# 🅿️ Parkchain - Decentralized Parking Space Rental Protocol

A blockchain-based parking space rental system built on Stacks that uses NFTs for time-bound access control to parking spaces.

## 🌟 Features

- **🏗️ Create Parking Spaces**: Property owners can list their parking spaces with custom pricing
- **🎫 NFT-Based Access**: Renters receive NFT passes that serve as digital parking permits
- **⏰ Time-Bound Rentals**: Automatic expiration of parking passes based on rental duration
- **💰 Flexible Pricing**: Hourly rates set by space owners
- **🔄 Rental Extensions**: Extend active rentals without losing your spot
- **📊 Earnings Tracking**: Track total earnings for space owners
- **🛡️ Platform Fees**: Built-in fee mechanism for platform sustainability

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to test and deploy

```bash
clarinet check
```

```bash
clarinet test
```

```bash
clarinet console
```

## 📖 Usage Guide

### For Parking Space Owners 🏠

#### Create a Parking Space
```clarity
(contract-call? .Parkchain create-parking-space "123 Main St, Spot A" u50)
```
- `location`: Description of parking space location (max 100 characters)
- `price-per-hour`: Price in microSTX per hour

#### Update Pricing
```clarity
(contract-call? .Parkchain update-space-price u1 u75)
```

#### Toggle Availability
```clarity
(contract-call? .Parkchain toggle-space-availability u1)
```

### For Renters 🚗

#### Rent a Parking Space
```clarity
(contract-call? .Parkchain rent-parking-space u1 u4)
```
- `space-id`: ID of the parking space
- `duration-hours`: Rental duration (1-24 hours)

#### Extend Your Rental
```clarity
(contract-call? .Parkchain extend-rental u1 u2)
```

#### End Rental Early
```clarity
(contract-call? .Parkchain end-rental u1)
```

### Read-Only Functions 📊

#### Get Parking Space Info
```clarity
(contract-call? .Parkchain get-parking-space u1)
```

#### Check Rental Status
```clarity
(contract-call? .Parkchain get-rental-info u1)
(contract-call? .Parkchain is-rental-active u1)
```

#### Calculate Costs
```clarity
(contract-call? .Parkchain calculate-rental-cost u1 u4)
```

#### Get User Data
```clarity
(contract-call? .Parkchain get-user-spaces 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(contract-call? .Parkchain get-user-rentals 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```
