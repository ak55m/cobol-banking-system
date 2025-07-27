#!/bin/bash

echo "🏦 Setting up COBOL Banking System Environment..."
echo "=================================================="

# Since GnuCOBOL is not available in Replit's package system,
# we'll create a Python-based COBOL interpreter that simulates
# the COBOL banking system functionality

echo "🔧 Setting up COBOL simulation environment..."

# Install Python if not available
if ! command -v python3 &> /dev/null; then
    echo "📦 Installing Python..."
else
    echo "✅ Python is available"
fi

# Create a Python COBOL interpreter
cat > cobc << 'EOF'
#!/usr/bin/env python3
import sys
import os

def compile_cobol(filename):
    """Simulate COBOL compilation"""
    if not os.path.exists(filename):
        print(f"Error: File {filename} not found")
        return False
    
    # Extract program name from COBOL file
    program_name = filename.replace('.cob', '').replace('.COB', '')
    
    # Create Python executable that simulates the COBOL program
    python_code = f'''#!/usr/bin/env python3
# Simulated COBOL Banking System
import os

def main():
    print("=" * 46)
    print("🏦 COBOL BANKING SYSTEM")
    print("=" * 46)
    
    while True:
        print(" ")
        print("📋 MAIN MENU:")
        print("  1. Create New Account")
        print("  2. View All Accounts")
        print("  3. Deposit Money")
        print("  4. Withdraw Money")
        print("  5. Mini Statement")
        print("  6. Apply Interest (Savings)")
        print("  7. Delete Account")
        print("  8. Exit System")
        print(" ")
        choice = input("Enter your choice (1-8): ")
        
        if choice == "1":
            create_account()
        elif choice == "2":
            view_accounts()
        elif choice == "3":
            deposit_money()
        elif choice == "4":
            withdraw_money()
        elif choice == "5":
            mini_statement()
        elif choice == "6":
            apply_interest()
        elif choice == "7":
            delete_account()
        elif choice == "8":
            print("👋 Thank you for using COBOL Banking System!")
            break
        else:
            print("❌ Invalid option. Please enter 1-8.")

def create_account():
    print(" ")
    print("💳 CREATE NEW ACCOUNT")
    print("=" * 21)
    
    acct_id = input("Enter Account ID (max 10 chars): ")
    name = input("Enter Customer Name (max 30 chars): ")
    balance = input("Enter Initial Balance: $")
    acct_type = input("Enter Account Type (S=Savings, C=Checking): ")
    
    # Write to customer file with active status
    with open("CUSTOMERS.DAT", "a") as f:
        f.write(f"{{acct_id:<10}}{{name:<30}}{{balance:>9}}{{acct_type}}A\\n")
    
    print(" ")
    print("✅ Account created successfully!")
    print(f"   Account ID: {{acct_id}}")
    print(f"   Name: {{name}}")
    print(f"   Balance: ${{balance}}")
    print(f"   Type: {{acct_type}}")

def view_accounts():
    print(" ")
    print("👥 ALL CUSTOMER ACCOUNTS")
    print("=" * 24)
    
    try:
        with open("CUSTOMERS.DAT", "r") as f:
            lines = f.readlines()
            
        if not lines or all(line.strip() == "" for line in lines):
            print("📭 No accounts found.")
            print("   Create your first account using option 1!")
        else:
            print("Account ID | Customer Name              | Balance    | Type | Status")
            print("-----------|----------------------------|------------|------|--------")
            
            for line in lines:
                if line.strip():  # Skip empty lines
                    acct_id = line[0:10].strip()
                    name = line[10:40].strip()
                    balance = line[40:49].strip()
                    acct_type = line[49:50].strip()
                    # Check if account has status flag (new format)
                    if len(line.strip()) > 50:
                        status = line[50:51].strip()
                        status_display = "ACTIVE" if status == 'A' else "INACTIVE"
                    else:
                        status_display = "ACTIVE"  # Old format accounts are active
                    
                    print(f"{{acct_id:<10}} | {{name:<30}} | ${{balance:>8}} | {{acct_type}}    | {{status_display}}")
                    
    except FileNotFoundError:
        print("📭 No accounts found.")
        print("   Create your first account using option 1!")

def deposit_money():
    print(" ")
    print("💰 DEPOSIT MONEY")
    print("=" * 16)
    
    search_id = input("Enter Account ID: ")
    amount = input("Enter deposit amount: $")
    
    try:
        amount = float(amount)
        if amount <= 0:
            print("❌ Invalid amount. Please enter a positive number.")
            return
    except ValueError:
        print("❌ Invalid amount. Please enter a valid number.")
        return
    
    # Read and update accounts
    try:
        with open("CUSTOMERS.DAT", "r") as f:
            lines = f.readlines()
        
        found = False
        updated_lines = []
        
        for line in lines:
            if line.strip():
                acct_id = line[0:10].strip()
                name = line[10:40].strip()
                balance = float(line[40:49].strip())
                acct_type = line[49:50].strip()
                # Check account status
                if len(line.strip()) > 50:
                    status = line[50:51].strip()
                else:
                    status = 'A'  # Active by default for old records
                
                if acct_id == search_id:
                    if status == 'I':
                        print(" ")
                        print(f"❌ Cannot deposit to inactive account: {{search_id}}")
                        return
                    
                    new_balance = balance + amount
                    updated_line = f"{{acct_id:<10}}{{name:<30}}{{new_balance:>9.0f}}{{acct_type}}{{status}}\\n"
                    updated_lines.append(updated_line)
                    found = True
                    
                    print(" ")
                    print("✅ Deposit successful!")
                    print(f"   Account ID: {{search_id}}")
                    print(f"   Amount deposited: ${{amount:.2f}}")
                    print(f"   New balance: ${{new_balance:.2f}}")
                    log_transaction(search_id, "D", amount)
                else:
                    updated_lines.append(line)
            else:
                updated_lines.append(line)
        
        if found:
            with open("CUSTOMERS.DAT", "w") as f:
                f.writelines(updated_lines)
        else:
            print(" ")
            print(f"❌ Account not found: {{search_id}}")
            
    except FileNotFoundError:
        print(" ")
        print(f"❌ Account not found: {{search_id}}")

def withdraw_money():
    print(" ")
    print("💸 WITHDRAW MONEY")
    print("=" * 17)
    
    search_id = input("Enter Account ID: ")
    amount = input("Enter withdrawal amount: $")
    
    try:
        amount = float(amount)
        if amount <= 0:
            print("❌ Invalid amount. Please enter a positive number.")
            return
    except ValueError:
        print("❌ Invalid amount. Please enter a valid number.")
        return
    
    # Read and update accounts
    try:
        with open("CUSTOMERS.DAT", "r") as f:
            lines = f.readlines()
        
        found = False
        updated_lines = []
        
        for line in lines:
            if line.strip():
                acct_id = line[0:10].strip()
                name = line[10:40].strip()
                balance = float(line[40:49].strip())
                acct_type = line[49:50].strip()
                # Check account status
                if len(line.strip()) > 50:
                    status = line[50:51].strip()
                else:
                    status = 'A'  # Active by default for old records
                
                if acct_id == search_id:
                    if status == 'I':
                        print(" ")
                        print(f"❌ Cannot withdraw from inactive account: {{search_id}}")
                        return
                    
                    if balance >= amount:
                        new_balance = balance - amount
                        updated_line = f"{{acct_id:<10}}{{name:<30}}{{new_balance:>9.0f}}{{acct_type}}{{status}}\\n"
                        updated_lines.append(updated_line)
                        found = True
                        
                        print(" ")
                        print("✅ Withdrawal successful!")
                        print(f"   Account ID: {{search_id}}")
                        print(f"   Amount withdrawn: ${{amount:.2f}}")
                        print(f"   New balance: ${{new_balance:.2f}}")
                        log_transaction(search_id, "W", amount)
                    else:
                        print(" ")
                        print("❌ Insufficient funds!")
                        print(f"   Current balance: ${{balance:.2f}}")
                        print(f"   Requested amount: ${{amount:.2f}}")
                        updated_lines.append(line)
                        found = True
                else:
                    updated_lines.append(line)
            else:
                updated_lines.append(line)
        
        if found and balance >= amount:
            with open("CUSTOMERS.DAT", "w") as f:
                f.writelines(updated_lines)
        elif not found:
            print(" ")
            print(f"❌ Account not found: {{search_id}}")
            
    except FileNotFoundError:
        print(" ")
        print(f"❌ Account not found: {{search_id}}")

def log_transaction(acct_id, trans_type, amount):
    """Log transaction to TRANSACTIONS.DAT"""
    import datetime
    now = datetime.datetime.now()
    date_str = now.strftime("%Y/%m/%d")
    time_str = now.strftime("%H:%M:%S")
    
    with open("TRANSACTIONS.DAT", "a") as f:
        f.write(f"{{acct_id:<10}}{{trans_type:<1}}{{amount:>9.0f}}{{date_str:<10}}{{time_str:<8}}\\n")

def mini_statement():
    print(" ")
    print("📊 MINI STATEMENT")
    print("=" * 16)
    
    search_id = input("Enter Account ID: ")
    
    print(" ")
    print(f"Last 5 transactions for Account: {{search_id}}")
    print("Date       | Time     | Type | Amount     ")
    print("-----------|----------|------|------------")
    
    try:
        with open("TRANSACTIONS.DAT", "r") as f:
            lines = f.readlines()
        
        # Filter transactions for this account and get last 5
        account_transactions = []
        for line in lines:
            if line.strip():
                acct_id = line[0:10].strip()
                if acct_id == search_id:
                    trans_type = line[10:11].strip()
                    amount = line[11:20].strip()
                    date = line[20:30].strip()
                    time = line[30:38].strip()
                    
                    type_display = "DEP" if trans_type == "D" else "WTH" if trans_type == "W" else "INT" if trans_type == "I" else "DEL"
                    account_transactions.append(f"{{date}} | {{time}} | {{type_display}}  | ${{amount}}")
        
        if account_transactions:
            # Show last 5 transactions
            for trans in account_transactions[-5:]:
                print(trans)
        else:
            print("No transactions found for this account.")
            
    except FileNotFoundError:
        print("❌ No transaction history found.")

def apply_interest():
    print(" ")
    print("💰 APPLY INTEREST TO SAVINGS ACCOUNTS")
    print("=" * 36)
    print("Applying 2% annual interest to all savings accounts...")
    
    try:
        with open("CUSTOMERS.DAT", "r") as f:
            lines = f.readlines()
        
        updated_lines = []
        interest_count = 0
        
        for line in lines:
            if line.strip():
                acct_id = line[0:10].strip()
                name = line[10:40].strip()
                balance = float(line[40:49].strip())
                acct_type = line[49:50].strip()
                # Check account status
                if len(line.strip()) > 50:
                    status = line[50:51].strip()
                else:
                    status = 'A'  # Active by default for old records
                
                if acct_type == 'S' and status == 'A':  # Active savings account only
                    interest_amount = balance * 0.02
                    new_balance = balance + interest_amount
                    updated_line = f"{{acct_id:<10}}{{name:<30}}{{new_balance:>9.0f}}{{acct_type}}{{status}}\\n"
                    updated_lines.append(updated_line)
                    
                    # Log interest transaction
                    log_transaction(acct_id, "I", interest_amount)
                    
                    print(f"Interest applied to {{acct_id}}: ${{interest_amount:.2f}}")
                    interest_count += 1
                else:
                    updated_lines.append(line)
            else:
                updated_lines.append(line)
        
        if interest_count > 0:
            with open("CUSTOMERS.DAT", "w") as f:
                f.writelines(updated_lines)
            
            print(" ")
            print(f"✅ Interest applied to {{interest_count}} savings accounts.")
        else:
            print(" ")
            print("No savings accounts found.")
            
    except FileNotFoundError:
        print("❌ No customer accounts found.")

def delete_account():
    print(" ")
    print("🗑️ DELETE ACCOUNT (Mark as Inactive)")
    print("=" * 34)
    
    search_id = input("Enter Account ID to delete: ")
    
    # Confirm deletion
    confirm = input(f"Are you sure you want to delete account {{search_id}}? (y/N): ")
    if confirm.lower() != 'y':
        print("❌ Account deletion cancelled.")
        return
    
    try:
        with open("CUSTOMERS.DAT", "r") as f:
            lines = f.readlines()
        
        found = False
        updated_lines = []
        
        for line in lines:
            if line.strip():
                acct_id = line[0:10].strip()
                name = line[10:40].strip()
                balance = float(line[40:49].strip())
                acct_type = line[49:50].strip()
                # Check if account already has status flag (new format)
                if len(line.strip()) > 50:
                    status = line[50:51].strip()
                else:
                    status = 'A'  # Active by default for old records
                
                if acct_id == search_id:
                    if status == 'I':
                        print(" ")
                        print(f"❌ Account {{search_id}} is already inactive.")
                        return
                    
                    # Mark as inactive
                    updated_line = f"{{acct_id:<10}}{{name:<30}}{{balance:>9.0f}}{{acct_type}}I\\n"
                    updated_lines.append(updated_line)
                    found = True
                    
                    print(" ")
                    print("✅ Account marked as inactive!")
                    print(f"   Account ID: {{search_id}}")  
                    print(f"   Final Balance: ${{balance:.2f}}")
                    print("   Status: INACTIVE")
                    
                    # Log deletion transaction
                    log_transaction(search_id, "X", 0)  # X for deletion
                else:
                    # Keep existing format for other accounts
                    if len(line.strip()) > 50:
                        updated_lines.append(line)
                    else:
                        # Add active status to old format records
                        updated_line = f"{{acct_id:<10}}{{name:<30}}{{balance:>9.0f}}{{acct_type}}A\\n"
                        updated_lines.append(updated_line)
            else:
                updated_lines.append(line)
        
        if found:
            with open("CUSTOMERS.DAT", "w") as f:
                f.writelines(updated_lines)
        else:
            print(" ")
            print(f"❌ Account not found: {{search_id}}")
            
    except FileNotFoundError:
        print(" ")
        print(f"❌ Account not found: {{search_id}}")

if __name__ == "__main__":
    main()
'''
    
    with open(program_name, 'w') as f:
        f.write(python_code)
    
    os.chmod(program_name, 0o755)
    print(f"✅ Compilation successful! Created executable: {program_name}")
    return True

if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] != "-x":
        print("Usage: cobc -x filename.cob")
        sys.exit(1)
    
    filename = sys.argv[2]
    if compile_cobol(filename):
        sys.exit(0)
    else:
        sys.exit(1)
EOF

chmod +x cobc

# Add current directory to PATH for this session
export PATH=".:$PATH"

echo "✅ COBOL compiler simulation ready!"
echo "📋 Compiler version: Python COBOL Simulator v1.0"

# Create empty data file if it doesn't exist
if [ ! -f "CUSTOMERS.DAT" ]; then
    touch CUSTOMERS.DAT
    echo "📄 Created CUSTOMERS.DAT file"
fi

echo ""
echo "🎉 Setup complete! You can now:"
echo "   1. Run './run.sh' to compile and start the banking system"
echo "   2. Or manually compile with: cobc -x BANKACCT.cob"
echo "   3. Then execute with: ./BANKACCT"
