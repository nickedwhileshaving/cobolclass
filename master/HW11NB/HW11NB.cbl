       IDENTIFICATION DIVISION.
       PROGRAM-ID.   HW11NB.
       AUTHOR.       NIKOLINA BEST. 
       DATE-WRITTEN. 03/23/18. 
       DATA DIVISION. 
       WORKING-STORAGE SECTION.
      
       01 NUM-IN          PIC XXXX.
          88 INTEGER-ENTERED       VALUE "0000" THRU "9999".
       01 NUM-IN-INT REDEFINES
          NUM-IN          PIC 9999.
       01 WS-REMAINDER    pic 9999 VALUE 1.
          88 NOT-PRIME-NUMBER      VALUE 0.
       01 WS-DIVISOR      pic 9999 VALUE 0.
       01 WS-QUOTIENT     pic 9999 VALUE 0.
        
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Nikolina Best".
           DISPLAY "List of Prime Numbers to 999".
       
           PERFORM UNTIL NUM-IN IS NUMERIC.
           PERFORM 1000-GET-INPUT-NUMBER
           ACCEPT NUM-IN
             
           
           PERFORM 2000-CHECK-THE-NUMBER 
             VARYING WS-DIVISOR FROM 2 BY 1
               UNTIL WS-REMAINDER = 0
                  OR WS-DIVISOR = (NUM-IN-INT - 1).
           
           IF NOT-PRIME-NUMBER
             DISPLAY NUM-IN " IS NOT A PRIME"
           ELSE  
             DISPLAY NUM-IN " IS A PRIME".
             
           GOBACK.
            
       1000-GET-INPUT-NUMBER.
         DISPLAY "ENTER INTEGER FROM 0000-9999 (WITH LEADING ZEROES)".
         ACCEPT NUM-IN.
    
       2000-CHECK-THE-NUMBER.
           DIVIDE NUM-IN-INT by WS-DIVISOR
             GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.