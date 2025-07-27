#!/bin/bash

echo "🏦 COBOL Banking System - Compile & Run"
echo "======================================="

# Add current directory to PATH
export PATH=".:$PATH"

# Check if COBOL compiler is available
if ! command -v cobc &> /dev/null; then
    echo "❌ COBOL compiler not found. Please run './setup.sh' first."
    exit 1
fi

# Check if COBOL source file exists
if [ ! -f "BANKACCT.cob" ]; then
    echo "❌ BANKACCT.cob not found in current directory."
    exit 1
fi

# Compile the COBOL program
echo "🔨 Compiling BANKACCT.cob..."
./cobc -x BANKACCT.cob

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "✅ Compilation successful!"
    echo ""
    echo "🚀 Starting Banking System..."
    echo ""
    # Run the compiled program
    ./BANKACCT
else
    echo "❌ Compilation failed. Please check your COBOL code."
    exit 1
fi
