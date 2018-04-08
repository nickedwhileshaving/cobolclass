        IDENTIFICATION DIVISION.
       PROGRAM-ID. COMMCALC.
      * ASK USER FOR THE NUMBER OF UNITS SOLD AND THE UNIT PRICE.
      * DISPLAY THE PRICE AND UNITS ENTERED. 
      * CALCULATE THE GROSS SALES AMOUNT (UNITS X PRICE).  
      * DISPLAY GROSS SALES.
      * CALCULATE THE COMMISSION (GROSS * COMM-RATE).
      * DISPLAY THE COMMISSION.
      * CALCULATE THE NET PROFIT FOR THE ITEM 
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
       01  WS-TODAYS-DATE          pic x(08).
       01  WS-TIME                 PIC 9(08).
       01  WS-END                  pic X(01).
       
       01  WS-UNITS-IN             PIC .....
       01  WS-UNITS-NUMERIC        PIC .....
       
       01  WS-UNIT-RATE-IN.
           05 WS-UNIT-RATE-DOLLARS  .........
           05 WS-UNIT-RATE-DECIMAL  .........
           05 WS-UNIT-RATE-CENTS    .........
       
       01  WS-NUMERIC-UNIT-PARTS.
           05 WS-NUMERIC-UNIT-DOLLARS 
                                    .........
           05 WS-NUMERIC-UNIT-CENTS .........
       01  WS-NUMERIC-UNIT-WHOLE    .........           
       
       01  WS-COMM-RATE            PIC ....     VALUE .09.

       01  WS-GROSS-AMOUNT         PIC .......  VALUE 0.
       01  WS-COMM-AMOUNT          PIC .......  VALUE 0.
       01  WS-NET-PROFIT           PIC 9(06)V99 VALUE 0.
       
       01  WS-FORMATTED-AMOUNT-Z   PIC ..............
       01  WS-FORMATTED-AMOUNT     PIC ..............       
       01  WS-FORMATTED-UNITS      PIC ....
       
       PROCEDURE DIVISION.
       0000-MAINLINE.
           
           DISPLAY 'START COMMCAFL BY YOUR NAME'.
           perform 1000-get-date-time.
        
      * PROMPT THE USER FOR RATE UNTIL NUMERIC RATE ENTERED: 
           PERFORM UNTIL ..................
                         ..................
                     AND WS-UNIT-RATE-DECIMAL = '.'
             DISPLAY "ENTER THE UNIT PRICE FROM 000.00 TO 999.99"
                     " AND PRESS ENTER"
             ACCEPT WS-UNIT-RATE-IN
           END-PERFORM
           
      * PROMPT THE USER FOR UNITS UNTIL NUMERIC UNITS ENTERED:          
           PERFORM UNTIL ........... IS NUMERIC
             DISPLAY "ENTER THE UNITS SOLD FROM 000 TO 999"
                     " AND PRESS ENTER"
             ACCEPT WS-UNITS-IN
           END-PERFORM           
      * CONVERT THE STRING DATA ENTERED INTO NUMERIC FIELD:
           .....
           .....
           .....
           .....
           .....
           display "______________________________".
           
      * FORMAT UNITS IN AND DISPLAY:
           .....
           .....       
           
      * FORMAT AND DISPLAY UNIT PRICE:            
           MOVE .............
           DISPLAY "RATE ENTERED : " WS-FORMATTED-AMOUNT.
        
      * COMPUTE, FORMAT AND DISPLAY GROSS SALES:             
           COMPUTE WS-GROSS-AMOUNT = ...............
           
           MOVE WS-GROSS-AMOUNT   TO ................
           DISPLAY "GROSS SALES  : " ................  
           
      * COMPUTE, FORMAT AND DISPLAY COMMISSION:             
           COMPUTE WS-COMM-AMOUNT = .................
                                        .