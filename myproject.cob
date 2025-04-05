       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTION-SUMMARY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "transactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "summary.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       *> Structure of each transaction record
       FD INPUT-FILE.
       01 IN-RECORD.
           05 TRX-ID        PIC X(13).
           05 SPACE1        PIC X.
           05 CUSTOMER-ID   PIC X(8).
           05 SPACE2        PIC X.
           05 TRX-TYPE      PIC X(10).
           05 SPACE3        PIC X.
           05 AMOUNT        PIC 9(5).
           05 SPACE4        PIC X.
           05 TIMESTAMP     PIC X(19).

       *> Output record format
       FD OUTPUT-FILE.
       01 OUT-RECORD        PIC X(100).

       WORKING-STORAGE SECTION.
       *> End-of-file flag with condition names
       01 EOF-FLAG           PIC X VALUE "N".
           88 END-OF-FILE    VALUE "Y".
           88 NOT-END        VALUE "N".
       *> Storage for current transaction's customer ID
       01 WS-CURRENT-ID      PIC X(8) VALUE "        ".
       *> Totals for deposits and withdrawals and transfers
       01 WS-TOTAL-DEPOSIT   PIC 9(6) VALUE 0.
       01 WS-TOTAL-WITHDRAW  PIC 9(6) VALUE 0.
       01 WS-TOTAL-TRANSFER  PIC 9(6) VALUE 0.
       *> Count of transactions per customer
       01 WS-TRX-COUNT       PIC 9(4) VALUE 0.
       *> Title provided by user for the report
       01 WS-REPORT-TITLE    PIC X(50).
       01 WS-LINE            PIC X(100).
       01 FILLER             PIC X(1).
       *> Temporary field for filtering date
       01 WS-FILTER-DATE     PIC X(10) VALUE "2024-01-01".
       *> Tracking the most active customer
       01 WS-MAX-COUNT       PIC 9(4) VALUE 0.
       01 WS-TOP-CUSTOMER    PIC X(8) VALUE "        ".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
       *> Prompt user for report title
           DISPLAY "Enter report title:"
           ACCEPT WS-REPORT-TITLE
           DISPLAY "Opening files..."
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE

       *> Read all records until EOF
           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TIMESTAMP(1:10) >= WS-FILTER-DATE
                           DISPLAY "Reading: " IN-RECORD
                           PERFORM PROCESS-RECORD
                       END-IF
               END-READ
           END-PERFORM

       *> Final customer summary
           IF WS-CURRENT-ID NOT = "        "
               PERFORM WRITE-SUMMARY
           END-IF

       *> Display most active customer
           MOVE SPACES TO OUT-RECORD
           STRING
               "Top Customer: " DELIMITED BY SIZE
               WS-TOP-CUSTOMER DELIMITED BY SPACE
               " | Transactions: " DELIMITED BY SIZE
               WS-MAX-COUNT DELIMITED BY SPACE
               INTO OUT-RECORD
           END-STRING
           WRITE OUT-RECORD
           DISPLAY "Most Active Customer: " WS-TOP-CUSTOMER
           DISPLAY "Transactions: " WS-MAX-COUNT

       *> Close files
           DISPLAY "Processing complete."
           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.

       PROCESS-RECORD.
           DISPLAY "Transaction type: " TRX-TYPE

       *> Normalize transaction type (case-insensitive match)
           IF TRX-TYPE(1:1) = "D"
               MOVE "DEPOSIT   " TO TRX-TYPE
           ELSE
               IF TRX-TYPE(1:1) = "W"
                   MOVE "WITHDRAWAL" TO TRX-TYPE
               ELSE
                   IF TRX-TYPE(1:1) = "T"
                       MOVE "TRANSFER  " TO TRX-TYPE
                   END-IF
               END-IF
           END-IF

       *> If new customer ID is found, summarize the previous one
           IF CUSTOMER-ID NOT = WS-CURRENT-ID AND
              WS-CURRENT-ID NOT = "        "
               PERFORM WRITE-SUMMARY
               IF WS-TRX-COUNT > WS-MAX-COUNT
                   MOVE WS-TRX-COUNT TO WS-MAX-COUNT
                   MOVE WS-CURRENT-ID TO WS-TOP-CUSTOMER
               END-IF
               MOVE 0 TO WS-TOTAL-DEPOSIT
               MOVE 0 TO WS-TOTAL-WITHDRAW
               MOVE 0 TO WS-TOTAL-TRANSFER
               MOVE 0 TO WS-TRX-COUNT
           END-IF

       *> Accumulate transaction amount by type
           IF TRX-TYPE = "DEPOSIT   "
               ADD AMOUNT TO WS-TOTAL-DEPOSIT
           ELSE
               IF TRX-TYPE = "WITHDRAWAL"
                   ADD AMOUNT TO WS-TOTAL-WITHDRAW
               ELSE
                   IF TRX-TYPE = "TRANSFER  "
                       ADD AMOUNT TO WS-TOTAL-TRANSFER
                   END-IF
               END-IF
           END-IF

           ADD 1 TO WS-TRX-COUNT
           MOVE CUSTOMER-ID TO WS-CURRENT-ID.

       WRITE-SUMMARY.
           MOVE SPACES TO OUT-RECORD
           STRING
               WS-CURRENT-ID DELIMITED BY SPACE
               " | Deposits: " DELIMITED BY SIZE
               WS-TOTAL-DEPOSIT DELIMITED BY SPACE
               " | Withdrawals: " DELIMITED BY SIZE
               WS-TOTAL-WITHDRAW DELIMITED BY SPACE
               " | Transfers: " DELIMITED BY SIZE
               WS-TOTAL-TRANSFER DELIMITED BY SPACE
               " | Count: " DELIMITED BY SIZE
               WS-TRX-COUNT DELIMITED BY SPACE
               INTO OUT-RECORD
           END-STRING
           WRITE OUT-RECORD
           DISPLAY "Summary for customer: " WS-CURRENT-ID
           DISPLAY "Title: " WS-REPORT-TITLE.
