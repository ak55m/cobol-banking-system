# COBOL Banking System

## Overview

This is a simple banking system implemented in COBOL using the GnuCOBOL compiler. The application provides basic banking functionality including account creation with persistent data storage through file-based operations. The system is designed to run in a console environment and uses traditional COBOL file handling for data persistence.

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