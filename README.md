# COBOL Banking System

A comprehensive banking system implemented in COBOL using GnuCOBOL simulator. This project demonstrates traditional mainframe banking operations with modern file-based data persistence.

## Features

### Core Banking Operations
- **Account Management**: Create new customer accounts with unique IDs
- **Account Viewing**: Display all customer accounts with status information
- **Money Transactions**: Deposit and withdraw funds with balance validation
- **Account Status**: Mark accounts as inactive (soft delete)

### Advanced Features
- **Transaction Logging**: Complete audit trail of all banking operations
- **Mini Statements**: View last 5 transactions for any account
- **Interest Calculation**: Automatic 2% annual interest for savings accounts
- **Data Validation**: Prevents overdrafts and transactions on inactive accounts

### Transaction Types
- **DEP**: Deposits
- **WTH**: Withdrawals  
- **INT**: Interest payments
- **DEL**: Account deletions

## System Architecture

### Technology Stack
- **Language**: COBOL (GnuCOBOL simulator)
- **Data Storage**: Flat files (.DAT format)
- **Interface**: Interactive console application
- **Platform**: Linux/Unix environments

### Data Files
- `CUSTOMERS.DAT`: Customer account information
- `TRANSACTIONS.DAT`: Complete transaction history

### File Structure
```
COBOL-Banking-System/
├── BANKACCT.cob          # Main COBOL source code
├── setup.sh              # Environment setup and Python simulator
├── run.sh                # Compilation and execution script
├── cobc                  # COBOL compiler simulator
├── CUSTOMERS.DAT         # Customer data (created at runtime)
├── TRANSACTIONS.DAT      # Transaction log (created at runtime)
└── README.md             # This file
```

## Quick Start

### Prerequisites
- Linux/Unix environment (Ubuntu/Debian recommended)
- Bash shell
- Python 3.x (automatically used for simulation)

### Installation & Setup
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/cobol-banking-system.git
   cd cobol-banking-system
   ```

2. Make scripts executable:
   ```bash
   chmod +x setup.sh run.sh
   ```

3. Run the setup:
   ```bash
   ./setup.sh
   ```

4. Start the banking system:
   ```bash
   ./run.sh
   ```

## Usage Guide

### Menu Options
1. **Create New Account**: Add new customer with account details
2. **View All Accounts**: Display all accounts with status
3. **Deposit Money**: Add funds to existing accounts
4. **Withdraw Money**: Remove funds (with balance validation)
5. **Mini Statement**: View recent transaction history
6. **Apply Interest**: Calculate 2% interest for savings accounts
7. **Delete Account**: Mark account as inactive
8. **Exit System**: Close the application

### Sample Workflow
```
1. Create a new savings account
2. Make some deposits and withdrawals
3. View mini statement to see transaction history
4. Apply interest to earn 2% on savings
5. View all accounts to see updated balances
```

## Data Format

### Customer Records
```
Field        | Position | Length | Description
-------------|----------|--------|------------------
Account ID   | 1-10     | 10     | Unique identifier
Customer Name| 11-40    | 30     | Full name
Balance      | 41-49    | 9      | Account balance
Account Type | 50       | 1      | S=Savings, C=Checking
Status       | 51       | 1      | A=Active, I=Inactive
```

### Transaction Records
```
Field        | Position | Length | Description
-------------|----------|--------|------------------
Account ID   | 1-10     | 10     | Account identifier
Trans Type   | 11       | 1      | D/W/I/X (Dep/Wth/Int/Del)
Amount       | 12-20    | 9      | Transaction amount
Date         | 21-30    | 10     | YYYY/MM/DD format
Time         | 31-38    | 8      | HH:MM:SS format
```

## Technical Details

### COBOL Simulation
This project uses a Python-based COBOL simulator instead of native GnuCOBOL due to Replit environment limitations. The simulator:
- Mimics traditional COBOL file handling patterns
- Implements sequential file organization
- Maintains COBOL-style data structures and operations

### Error Handling
- Account not found validation
- Insufficient funds prevention
- Inactive account transaction blocking
- Invalid input data validation

### Data Persistence
All data is stored in flat files and persists across program restarts, simulating traditional mainframe data storage patterns.

## Development

### Project Structure
The banking system follows traditional COBOL program organization:
- **Identification Division**: Program metadata
- **Environment Division**: File control setup
- **Data Division**: Record layouts and variables
- **Procedure Division**: Business logic implementation

### Extending the System
To add new features:
1. Update the menu system in the main loop
2. Implement new functions following existing patterns
3. Add appropriate error handling and validation
4. Update transaction logging if needed

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/new-feature`)
5. Create a Pull Request

## License

This project is open source and available under the [MIT License](LICENSE).

## Acknowledgments

- Built using GnuCOBOL concepts and traditional mainframe banking patterns
- Designed for educational purposes and COBOL learning
- Demonstrates file-based data persistence in banking applications

---

**Note**: This is a simulation for educational purposes. Not intended for production banking use.