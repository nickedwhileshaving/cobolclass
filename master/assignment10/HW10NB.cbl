       IDENTIFICATION DIVISION.
       PROGRAM-ID.   HW10NB.
       AUTHOR.       NIKOLINA BEST.
      * CONVERT THE IN-LINE PERFORMS INTO PARAGRAPHS.
       DATE-WRITTEN. 03/23/18.

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
           DISPLAY "Prime Number Checking Program".
           PERFORM 1100-GET-USER-INPUT.
           PERFORM 1000-CHECK-NUMBER
             UNTIL USER-WANTS-TO-QUIT.
           DISPLAY "EXIT".
           GOBACK.
           
       1000-CHECK-NUMBER.
           MOVE 1 TO WS-REMAINDER.
           PERFORM 1010-DIVISION-CALC.

       1010-DIVISION-CALC.
           PERFORM VARYING WS-DIVISOR FROM 2 BY 1 
             UNTIL WS-REMAINDER = 0
                OR WS-DIVISOR = (NUM-IN-INT - 1)
                OR NUM-IN-INT = 1
               DIVIDE NUM-IN-INT by WS-DIVISOR 
                 GIVING WS-QUOTIENT REMAINDER WS-REMAINDER
           END-PERFORM
           PERFORM 1020-PRIME-CHK.

       1020-PRIME-CHK.
           IF NOT-PRIME-NUMBER OR NUM-IN-INT = 1
             DISPLAY NUM-IN " IS NOT A PRIME"
           ELSE  
             DISPLAY NUM-IN " IS A PRIME".
             
           MOVE SPACES TO NUM-IN.
           PERFORM 1100-GET-USER-INPUT.
           PERFORM 1110-INPUT-CHECK.

       1100-GET-USER-INPUT.
           PERFORM 1110-INPUT-CHECK.

       1110-INPUT-CHECK.
           PERFORM until NUM-IN is numeric OR USER-WANTS-TO-QUIT
             display "ENTER INTEGER 0000-9999 (WITH LEADING ZEROES)"
             display "(OR ENTER X TO QUIT)."
             ACCEPT NUM-IN
           END-PERFORM