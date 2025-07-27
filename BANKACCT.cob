       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKACCT.
       AUTHOR. BANKING-SYSTEM.
       DATE-WRITTEN. 2025-07-27.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMERS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 ACCT-ID     PIC X(10).
           05 NAME        PIC X(30).
           05 BALANCE     PIC 9(7)V99.
           05 ACCT-TYPE   PIC X(1).

       WORKING-STORAGE SECTION.
       01 CHOICE           PIC 9.
       01 WS-DONE          PIC X VALUE 'N'.
       01 FILE-STATUS      PIC XX.

       01 WS-ACCT-ID       PIC X(10).
       01 WS-NAME          PIC X(30).
       01 WS-BALANCE       PIC 9(7)V99.
       01 WS-TYPE          PIC X(1).
       
       01 WS-SEARCH-ID     PIC X(10).
       01 WS-AMOUNT        PIC 9(7)V99.
       01 WS-FOUND         PIC X VALUE 'N'.
       01 WS-NEW-BALANCE   PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=============================================="
           DISPLAY "🏦 COBOL BANKING SYSTEM"
           DISPLAY "=============================================="
           PERFORM UNTIL WS-DONE = 'Y'
               DISPLAY " "
               DISPLAY "📋 MAIN MENU:"
               DISPLAY "  1. Create New Account"
               DISPLAY "  2. View All Accounts"
               DISPLAY "  3. Deposit Money"
               DISPLAY "  4. Withdraw Money"
               DISPLAY "  5. Exit System"
               DISPLAY " "
               DISPLAY "Enter your choice (1-5): " WITH NO ADVANCING
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1
                       PERFORM CREATE-ACCOUNT
                   WHEN 2
                       PERFORM VIEW-ACCOUNTS
                   WHEN 3
                       PERFORM DEPOSIT-MONEY
                   WHEN 4
                       PERFORM WITHDRAW-MONEY
                   WHEN 5
                       DISPLAY "👋 Thank you for using COBOL Banking System!"
                       MOVE 'Y' TO WS-DONE
                   WHEN OTHER
                       DISPLAY "❌ Invalid option. Please enter 1-5."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       CREATE-ACCOUNT.
           DISPLAY " "
           DISPLAY "💳 CREATE NEW ACCOUNT"
           DISPLAY "====================="
           
           DISPLAY "Enter Account ID (max 10 chars): " WITH NO ADVANCING
           ACCEPT WS-ACCT-ID
           
           DISPLAY "Enter Customer Name (max 30 chars): " WITH NO ADVANCING
           ACCEPT WS-NAME
           
           DISPLAY "Enter Initial Balance: $" WITH NO ADVANCING
           ACCEPT WS-BALANCE
           
           DISPLAY "Enter Account Type (S=Savings, C=Checking): " 
               WITH NO ADVANCING
           ACCEPT WS-TYPE

           PERFORM WRITE-CUSTOMER-RECORD
           
           DISPLAY " "
           DISPLAY "✅ Account created successfully!"
           DISPLAY "   Account ID: " WS-ACCT-ID
           DISPLAY "   Name: " WS-NAME
           DISPLAY "   Balance: $" WS-BALANCE
           DISPLAY "   Type: " WS-TYPE.

       VIEW-ACCOUNTS.
           DISPLAY " "
           DISPLAY "👥 ALL CUSTOMER ACCOUNTS"
           DISPLAY "========================"
           
           OPEN INPUT CUSTOMER-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
               DISPLAY "   No accounts found or file cannot be read."
           ELSE
               DISPLAY "Account ID | Customer Name              | Balance    | Type"
               DISPLAY "-----------|----------------------------|------------|-----"
               
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       DISPLAY ACCT-ID " | " NAME " | $" BALANCE " | " ACCT-TYPE
                   END-IF
               END-PERFORM
               
               IF FILE-STATUS NOT = "10" AND FILE-STATUS NOT = "00"
                   DISPLAY "❌ Error reading customer file: " FILE-STATUS
               END-IF
           END-IF
           
           CLOSE CUSTOMER-FILE.

       DEPOSIT-MONEY.
           DISPLAY " "
           DISPLAY "💰 DEPOSIT MONEY"
           DISPLAY "================"
           
           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           DISPLAY "Enter deposit amount: $" WITH NO ADVANCING
           ACCEPT WS-AMOUNT
           
           PERFORM UPDATE-BALANCE-ADD
           
           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY "✅ Deposit successful!"
               DISPLAY "   Account ID: " WS-SEARCH-ID
               DISPLAY "   Amount deposited: $" WS-AMOUNT
               DISPLAY "   New balance: $" WS-NEW-BALANCE
           ELSE
               DISPLAY " "
               DISPLAY "❌ Account not found: " WS-SEARCH-ID
           END-IF.

       WITHDRAW-MONEY.
           DISPLAY " "
           DISPLAY "💸 WITHDRAW MONEY"
           DISPLAY "================="
           
           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           DISPLAY "Enter withdrawal amount: $" WITH NO ADVANCING
           ACCEPT WS-AMOUNT
           
           PERFORM UPDATE-BALANCE-SUBTRACT
           
           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY "✅ Withdrawal successful!"
               DISPLAY "   Account ID: " WS-SEARCH-ID
               DISPLAY "   Amount withdrawn: $" WS-AMOUNT
               DISPLAY "   New balance: $" WS-NEW-BALANCE
           ELSE
               DISPLAY " "
               DISPLAY "❌ Account not found: " WS-SEARCH-ID
           END-IF.

       UPDATE-BALANCE-ADD.
           MOVE 'N' TO WS-FOUND
           OPEN I-O CUSTOMER-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       IF ACCT-ID = WS-SEARCH-ID
                           ADD WS-AMOUNT TO BALANCE
                           MOVE BALANCE TO WS-NEW-BALANCE
                           REWRITE CUSTOMER-RECORD
                           MOVE 'Y' TO WS-FOUND
                           MOVE "10" TO FILE-STATUS
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           
           CLOSE CUSTOMER-FILE.

       UPDATE-BALANCE-SUBTRACT.
           MOVE 'N' TO WS-FOUND
           OPEN I-O CUSTOMER-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               PERFORM UNTIL FILE-STATUS = "10"
                   READ CUSTOMER-FILE
                   IF FILE-STATUS = "00"
                       IF ACCT-ID = WS-SEARCH-ID
                           IF BALANCE >= WS-AMOUNT
                               SUBTRACT WS-AMOUNT FROM BALANCE
                               MOVE BALANCE TO WS-NEW-BALANCE
                               REWRITE CUSTOMER-RECORD
                               MOVE 'Y' TO WS-FOUND
                           ELSE
                               DISPLAY " "
                               DISPLAY "❌ Insufficient funds!"
                               DISPLAY "   Current balance: $" BALANCE
                               DISPLAY "   Requested amount: $" WS-AMOUNT
                               MOVE 'N' TO WS-FOUND
                           END-IF
                           MOVE "10" TO FILE-STATUS
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           
           CLOSE CUSTOMER-FILE.

       WRITE-CUSTOMER-RECORD.
           OPEN EXTEND CUSTOMER-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "❌ Error opening customer file: " FILE-STATUS
           ELSE
               MOVE WS-ACCT-ID TO ACCT-ID
               MOVE WS-NAME TO NAME
               MOVE WS-BALANCE TO BALANCE
               MOVE WS-TYPE TO ACCT-TYPE
               WRITE CUSTOMER-RECORD
               
               IF FILE-STATUS NOT = "00"
                   DISPLAY "❌ Error writing to customer file: " FILE-STATUS
               END-IF
           END-IF
           
           CLOSE CUSTOMER-FILE.
