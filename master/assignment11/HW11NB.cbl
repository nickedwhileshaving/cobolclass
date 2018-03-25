       IDENTIFICATION DIVISION.
       PROGRAM-ID.   HW11NB.
       AUTHOR.       NIKOLINA BEST.
      * CONVERT THE IN-LINE PERFORMS INTO PARAGRAPHS.
       DATE-WRITTEN. 03/25/18.

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       01 NUM-IN          PIC XXXX.
          88 USER-WANTS-TO-QUIT    VALUE "x" "X" "XX" "xx".
       01 NUM-IN-INT REDEFINES
          NUM-IN          PIC 9999.
       01 WS-QUOTIENT     PIC 9999
                                   VALUE 0.
       01 WS-REMAINDER    PIC 9999 VALUE 1.
          88 NOT-PRIME-NUMBER      VALUE 0.
       01 WS-DIVISOR      PIC 9999 VALUE 0.
        
       PROCEDURE DIVISION.
       0000-DRIVER.
           DISPLAY "NIKOLINA BEST".
           DISPLAY "List of Prime Numbers to 999:".
           PERFORM VARYING NUM-IN-INT FROM 1 BY 1
               UNTIL NUM-IN-INT = 1000
                   PERFORM 1000-CHECK-NUMBER
      *            DISPLAY NUM-IN-INT
           END-PERFORM.
           DISPLAY "END OF THE PROGRAM".
           ACCEPT NUM-IN.
           GOBACK.
           
       1000-CHECK-NUMBER.
           MOVE 1 TO WS-REMAINDER.
           PERFORM 1010-DIVISION-CALC.

       1010-DIVISION-CALC.
           PERFORM VARYING WS-DIVISOR FROM 2 BY 1 
             UNTIL WS-REMAINDER = 0
                OR WS-DIVISOR = (NUM-IN-INT - 1)
                OR NUM-IN-INT = 1
               MOVE 1 TO WS-REMAINDER
               DIVIDE NUM-IN-INT by WS-DIVISOR 
                 GIVING WS-QUOTIENT REMAINDER WS-REMAINDER
           END-PERFORM
           PERFORM 1020-PRIME-CHK.

       1020-PRIME-CHK.
           IF NOT-PRIME-NUMBER OR NUM-IN-INT = 1
           ELSE  
             DISPLAY NUM-IN-INT.
       