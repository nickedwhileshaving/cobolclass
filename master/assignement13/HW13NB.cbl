       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW13NB.
       AUTHOR. NIKOLINA BEST.
       DATE-WRITTEN. 04/08/18.

      * ASK USER FOR THE NUMBER OF UNITS SOLD AND THE UNIT PRICE.
      * DISPLAY THE PRICE AND UNITS ENTERED. 
      * CALCULATE THE GROSS SALES AMOUNT (UNITS X PRICE).  
      * DISPLAY GROSS SALES.
      * CALCULATE THE COMMISSION (GROSS * COMM-RATE).
      * DISPLAY THE COMMISSION.
      * CALCULATE THE NET PROFIT FOR THE ITEM
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
       01  WS-TODAYS-DATE             PIC X(08).
       01  WS-TIME                    PIC 9(08).
       01  WS-END                     PIC X(01).
       
       01  WS-UNITS-IN                PIC X(03).
       01  WS-UNITS-NUMERIC           PIC 9(03).
       
       01  WS-UNIT-RATE-IN.
           05 WS-UNIT-RATE-DOLLARS    PIC X(3).
           05 WS-UNIT-RATE-DECIMAL    PIC X.
           05 WS-UNIT-RATE-CENTS      PIC X(2).
       01  WS-UNIT-RATE-DUMMY-FIVE    PIC X(5).
       01  WS-NUMERIC-RATE            PIC 999V99 VALUE 0.00.
       
       01  WS-COMM-RATE               PIC V99 VALUE .09.
       01  WS-COMM-PERCENT            PIC 99.

       01  WS-GROSS-SALES             PIC 9(4)V99 VALUE 0.
       01  WS-COMM-AMOUNT             PIC 999V99 VALUE 0.
       01  WS-NET-PROFIT              PIC 9(6)V99 VALUE 0.

       01  WS-UNITS-SOLD-FORM         PIC ZZZZZZZZZZZZZ9.
       01  WS-RATE-ENTERED-FORM       PIC $ZZZZZZZZZZ.99.     
       01  WS-GROSS-SALES-FORM        PIC ZZZZZZZZZZZ.99.
       01  WS-COMMISSION-FORM         PIC ZZZZZZZZZZZ.99.
       01  WS-PROFIT-AMOUNT-FORM      PIC $ZZZZZZZZZZ.99.
       01  WS-COMM-PERCENT-FORM       PIC ZZZZZZZZZZZ99.

       01  WS-DASHED-LINE             PIC X(30)
                                 VALUE '______________________________'.
       01  WS-DATE-TIME-LINE.
           05 WS-DATE-TIME-BEGIN      PIC X(10) 
                                 VALUE ' ON DATE: '.
           05 WS-DATE-LINE-DATE       PIC X(8).
           05 WS-DATE-TIME-MIDDLE     PIC X(5)
                                 VALUE ' AT: '.
           05 WS-DATE-LINE-TIME       PIC X(8).

       PROCEDURE DIVISION.
       0000-MAINLINE.
           DISPLAY 'START HW13NB BY NIKI BEST.'.
           PERFORM 1000-GET-DATE-TIME.
           PERFORM 1100-GET-USER-RATE.
           PERFORM 1200-GET-USER-UNITS.
           PERFORM 1300-CONVERT-TO-NUMERICS.
           PERFORM 2000-DISPLAY-VALUES.
           GOBACK.
        
       1000-GET-DATE-TIME.
             MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATE-LINE-DATE.                                
             MOVE FUNCTION CURRENT-DATE (9:8) TO WS-DATE-LINE-TIME.

       1100-GET-USER-RATE.
      * PROMPT THE USER FOR RATE UNTIL NUMERIC RATE ENTERED: 
           PERFORM UNTIL WS-UNIT-RATE-DOLLARS IS NUMERIC
                     AND WS-UNIT-RATE-CENTS IS NUMERIC
                     AND WS-UNIT-RATE-DECIMAL = '.'
             DISPLAY "ENTER THE UNIT PRICE FROM 000.00 TO 999.99"
                     " AND PRESS ENTER"
             ACCEPT WS-UNIT-RATE-IN
           END-PERFORM.
       
       1200-GET-USER-UNITS.
      * PROMPT THE USER FOR UNITS UNTIL NUMERIC UNITS ENTERED:          
           PERFORM UNTIL WS-UNITS-IN IS NUMERIC
             DISPLAY "ENTER THE UNITS SOLD FROM 000 TO 999"
                     " AND PRESS ENTER"
             ACCEPT WS-UNITS-IN
           END-PERFORM.

       1300-CONVERT-TO-NUMERICS.
      * CONVERT THE STRING DATA ENTERED INTO NUMERIC FIELD:
           MOVE WS-UNITS-IN TO WS-UNITS-NUMERIC.
           STRING WS-UNIT-RATE-DOLLARS
                  WS-UNIT-RATE-CENTS 
           INTO WS-UNIT-RATE-DUMMY-FIVE.
           MOVE WS-UNIT-RATE-DUMMY-FIVE TO WS-NUMERIC-RATE.

       2000-DISPLAY-VALUES.
           DISPLAY WS-DASHED-LINE.
      * FORMAT UNITS IN AND DISPLAY:
           MOVE WS-UNITS-NUMERIC TO WS-UNITS-SOLD-FORM.
           DISPLAY 'UNITS SOLD   : ' WS-UNITS-SOLD-FORM.

      * FORMAT AND DISPLAY UNIT PRICE:            
           MOVE WS-NUMERIC-RATE TO WS-RATE-ENTERED-FORM.
           DISPLAY 'RATE ENTERED : ' WS-RATE-ENTERED-FORM.
        
      * COMPUTE, FORMAT AND DISPLAY GROSS SALES:             
           COMPUTE WS-GROSS-SALES = WS-NUMERIC-RATE
                           * WS-UNITS-NUMERIC.
           MOVE WS-GROSS-SALES   TO WS-GROSS-SALES-FORM.
           DISPLAY 'GROSS SALES  : ' WS-GROSS-SALES-FORM.
           
      * COMPUTE, FORMAT AND DISPLAY COMMISSION:             
           COMPUTE WS-COMM-AMOUNT = WS-GROSS-SALES 
                           * WS-COMM-RATE.
           MOVE WS-COMM-AMOUNT TO WS-COMMISSION-FORM.
           DISPLAY 'COMMISSION   : ' WS-COMMISSION-FORM.

           COMPUTE WS-NET-PROFIT = WS-GROSS-SALES
                           - WS-COMM-AMOUNT.
           MOVE WS-NET-PROFIT TO WS-PROFIT-AMOUNT-FORM
           DISPLAY 'PROFIT AMOUNT: ' WS-PROFIT-AMOUNT-FORM.
           COMPUTE WS-COMM-PERCENT = WS-COMM-RATE * 100.
           MOVE WS-COMM-PERCENT TO WS-COMM-PERCENT-FORM.
           DISPLAY 'COMMISSION   : %' WS-COMM-PERCENT-FORM.
           DISPLAY WS-DASHED-LINE.
           DISPLAY 'PROGRAM ENDS'.
           DISPLAY WS-DATE-TIME-LINE.