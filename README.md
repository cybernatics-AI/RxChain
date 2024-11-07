# Pharmaceutical Supply Chain Smart Contract

## Overview

This Clarity smart contract implements a comprehensive pharmaceutical supply chain management system on the Stacks blockchain. It provides functionality for tracking drug batches from manufacture to dispensing, including quality control, ownership transfers, and regulatory actions.

## Features

- Batch registration with detailed information
- Ownership transfer of entire batches or partial quantities
- Quality metric recording and validation
- Medication dispensing with prescription tracking
- Batch recall and destruction capabilities
- Comprehensive data retrieval functions

## Key Components

### Roles

- Manufacturer (1)
- Distributor (2)
- Pharmacy (3)
- Regulator (4)
- Quality Inspector (5)

### Batch Statuses

- Created (1)
- Approved (2)
- In Transit (3)
- Delivered (4)
- Dispensed (5)
- Recalled (6)
- Destroyed (7)

### Main Data Structures

1. `drug-batches`: Stores comprehensive information about each drug batch.
2. `batch-transfers`: Tracks the transfer history of batches.
3. `dispensing-records`: Records the dispensing history of medications.

## Key Functions

### For Manufacturers

- `register-batch`: Register a new drug batch with detailed information.

### For Distributors

- `transfer-batch-quantity`: Transfer a portion of a batch to another owner.
- `transfer-batch`: Transfer ownership of an entire batch.

### For Pharmacies

- `dispense-medication`: Record the dispensing of medication to a patient.

### For Quality Inspectors

- `record-quality-reading`: Record and validate quality metrics for a batch.

### For Regulators

- `recall-batch-with-reason`: Initiate a recall of a specific batch with a reason.
- `destroy-batch`: Mark a recalled batch as destroyed.

### Read-Only Functions

- `get-dispensing-history`: Retrieve the dispensing history of a batch.
- `get-transfer-history`: Get the transfer history of a batch.
- `get-batch-details`: Fetch comprehensive details of a specific batch.
- `is-batch-recalled`: Check if a batch has been recalled.
- `get-batch-status`: Get the current status of a batch.

## Error Handling

The contract defines various error codes for different scenarios, ensuring proper validation and error reporting throughout the supply chain process.

## Integration

This contract is designed to integrate with other contracts through traits:

- `auth-trait`: For authorization and role verification.
- `quality-trait`: For additional quality control measures.
- `batch-tracker-trait`: For external batch tracking capabilities.

## Usage

To use this contract, deploy it to the Stacks blockchain and interact with it using a Clarity-compatible wallet or through other smart contracts that implement the required traits.

## Security Considerations

- Ensure proper access control by implementing the `auth-trait` correctly.
- Regularly audit and update allowed principals for each role.
- Monitor quality readings and respond promptly to any out-of-range values.

## Future Enhancements

- Implement a more granular role-based access control system.
- Add support for batch splitting and merging.
- Integrate with IoT devices for automated quality readings.
- Implement a patient feedback system for pharmacovigilance.
