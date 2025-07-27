# COBOL Banking System

## Overview

This is a complete banking system implemented in COBOL using a Python-based GnuCOBOL simulator. The application provides comprehensive banking functionality including account creation, viewing accounts, depositing money, and withdrawing money with proper balance validation. All data is persistently stored through file-based operations. The system is designed to run in a console environment and uses traditional COBOL file handling patterns for data persistence.

## Recent Changes

**July 27, 2025 - Enhanced Banking System with Complete Transaction Management:**
- Enhanced COBOL program with deposit money functionality
- Enhanced COBOL program with withdraw money functionality  
- Added balance validation to prevent overdrafts
- Added proper error handling for account not found scenarios
- Implemented transaction logging system (TRANSACTIONS.DAT file)
- Added mini statement feature showing last 5 transactions per account
- Added interest calculation for savings accounts (2% annual rate)
- Added delete account functionality (marks accounts as inactive)
- Enhanced account status tracking (Active/Inactive)
- Added safeguards preventing transactions on inactive accounts
- Updated menu system with options 3-7 for all banking operations
- Updated Python simulator to handle all financial transactions with proper validation

## User Preferences

Preferred communication style: Simple, everyday language.

## System Architecture

The banking system follows a traditional mainframe-style architecture typical of COBOL applications:

- **Language**: COBOL with GnuCOBOL compiler
- **Data Storage**: File-based using sequential files (.DAT format)
- **Interface**: Interactive console/terminal interface
- **Deployment**: Shell script-based setup and execution

## Key Components

### 1. COBOL Program Structure
- **IDENTIFICATION DIVISION**: Program metadata and identification
- **ENVIRONMENT DIVISION**: File control and system environment configuration
- **DATA DIVISION**: Data structures and file record layouts
- **PROCEDURE DIVISION**: Business logic and program flow

### 2. File Management
- **CUSTOMERS.DAT**: Primary data file for storing customer account information
- **Line Sequential Organization**: Traditional COBOL file access method for simple read/write operations

### 3. Setup and Execution Scripts
- **setup.sh**: Environment preparation and GnuCOBOL installation script
- Automated compilation and execution workflow

## Data Flow

1. **Account Creation**: User inputs account information through console interface
2. **Data Validation**: COBOL program validates input data
3. **File Operations**: Account data is written to CUSTOMERS.DAT file
4. **Persistence**: Data remains available across program executions
5. **Interactive Loop**: Program continues accepting user input until termination

## External Dependencies

### Required Software
- **GnuCOBOL (open-cobol)**: COBOL compiler and runtime environment
- **Bash Shell**: For setup and execution scripts
- **Linux/Unix Environment**: Required for GnuCOBOL operation

### System Requirements
- Ubuntu/Debian-based system (for apt package management)
- File system write permissions for data file creation
- Terminal/console access for interactive operation

## Deployment Strategy

### Local Development
1. Execute `chmod +x setup.sh` to make setup script executable
2. Run `./setup.sh` to install GnuCOBOL and configure environment
3. Compile COBOL source files using `cobc` compiler
4. Execute compiled programs directly from command line

### Replit Deployment
- Uses Bash template as base environment
- Installs GnuCOBOL via `apt` package manager
- Configured for immediate execution in browser-based terminal
- No additional server configuration required

### Architecture Rationale
- **File-based storage**: Simple, traditional approach suitable for small-scale banking operations without database overhead
- **Sequential file organization**: Provides straightforward data access patterns typical of COBOL applications
- **Console interface**: Maintains simplicity and focuses on core banking logic rather than complex UI
- **Shell script automation**: Ensures consistent environment setup across different deployment targets