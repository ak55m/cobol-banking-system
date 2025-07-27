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

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=============================================="
           DISPLAY "🏦 COBOL BANKING SYSTEM"
           DISPLAY "=============================================="
           PERFORM UNTIL WS-DONE = 'Y'
               DISPLAY " "
               DISPLAY "📋 MAIN MENU:"
               DISPLAY "  1. Create New Account"
               DISPLAY "  2. Exit System"
               DISPLAY " "
               DISPLAY "Enter your choice (1-2): " WITH NO ADVANCING
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1
                       PERFORM CREATE-ACCOUNT
                   WHEN 2
                       DISPLAY "👋 Thank you for using COBOL Banking System!"
                       MOVE 'Y' TO WS-DONE
                   WHEN OTHER
                       DISPLAY "❌ Invalid option. Please enter 1 or 2."
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
