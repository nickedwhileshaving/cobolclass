       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW12NB.
       AUTHOR. NIKOLINA BEST.
       DATE-WRITTEN. 03/31/18.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARIABLES.
           05 BOTTLES              PIC S99     VALUE 0.
           05 REMAINING-BOTTLES    PIC S99     VALUE 0.
           05 COUNTING             PIC 99      VALUE 0.
           05 STARTING-POSITION    PIC 99      VALUE 0.
           05 POSITIONS            PIC 99      VALUE 0.
           05 NUM-IN               PIC X(2).
               88 IS-VALID             VALUE "01" THRU "99".
           05 NUM-IN-INT REDEFINES
              NUM-IN                   PIC 99.
           05 NUM-IN-DECR              PIC X(2).
              88 IS-VALID-DECR             VALUE "01" THRU "99".
           05 NUM-IN-INT-DECR REDEFINES
              NUM-IN-DECR             PIC S99.
           05 NUM-IN-TEXT             PIC X(2).

           
       PROCEDURE DIVISION.
           DISPLAY "PLEASE ENTER A STARTING NUMBER OF BOTTLES"
           PERFORM 1000-INPUT-CHECK.
           DISPLAY "PLEASE ENTER THE DECREMENTER"
           PERFORM 2000-INPUT-CHECK.
           MOVE NUM-IN-DECR TO NUM-IN-TEXT.
           MULTIPLY -1 BY NUM-IN-INT-DECR.
           PERFORM VARYING BOTTLES FROM NUM-IN-INT BY NUM-IN-INT-DECR UNTIL
               BOTTLES < NUM-IN-INT-DECR
               DISPLAY SPACES
               ADD NUM-IN-INT-DECR TO BOTTLES GIVING REMAINING-BOTTLES
               EVALUATE BOTTLES
                   WHEN 1
                       DISPLAY "1 BOTTLE OF BEER ON THE WALL, "
                               "1 BOTTLE OF BEER."
                       DISPLAY "TAKE ONE DOWN, PASS IT AROUND, "
                               "NO MORE BOTTLES OF BEER ON THE WALL."

                   WHEN 2 THRU 99
                       MOVE ZEROS TO COUNTING
                       INSPECT BOTTLES
                         TALLYING COUNTING FOR LEADING ZEROS
                       ADD 1 TO COUNTING GIVING STARTING-POSITION
                       SUBTRACT COUNTING FROM 2 GIVING POSITIONS
                       DISPLAY BOTTLES (STARTING-POSITION:POSITIONS)
                               " BOTTLES OF BEER ON THE WALL, "
                               BOTTLES (STARTING-POSITION:POSITIONS)
                               " BOTTLES OF BEER."
                       MOVE ZEROS TO COUNTING
                       INSPECT REMAINING-BOTTLES TALLYING
                         COUNTING FOR LEADING ZEROS
                       ADD 1 TO COUNTING GIVING STARTING-POSITION
                       SUBTRACT COUNTING FROM 2 GIVING POSITIONS
                       IF REMAINING-BOTTLES < 0 THEN
                              DISPLAY "TAKE " NUM-IN-TEXT " ONE DOWN, "
                              "PASS IT AROUND, NEGATIVE"
                               " BOTTLES OF BEER ON THE WALL."
                       ELSE
                           IF REMAINING-BOTTLES = 0 THEN
                           DISPLAY "TAKE " NUM-IN-TEXT " ONE DOWN, "
                                      "PASS IT AROUND, ZERO "
                                      "BOTTLES OF BEER ON THE WALL."
                           ELSE
                           DISPLAY "TAKE " NUM-IN-TEXT " ONE DOWN, "
                                      "PASS IT AROUND, "
                                       REMAINING-BOTTLES
                                       (STARTING-POSITION:POSITIONS)
                                       " BOTTLES OF BEER ON THE WALL."
                           END-IF
                       END-IF

               END-EVALUATE
           END-PERFORM
           DISPLAY "NO MORE BOTTLES OF BEER ON THE WALL, "
                   "NO MORE BOTTLES OF BEER..."
           DISPLAY "GET THE HAT AND PASS IT AROUND, "
                   "TIME TO B DOUBLE E DOUBLE R U-N, "
                   "BEER RUN."
           ACCEPT POSITIONS.
           STOP RUN.

       1000-INPUT-CHECK.
           PERFORM until IS-VALID
             DISPLAY "ENTER INTEGER IN RANGE 00-99"
             ACCEPT NUM-IN
           END-PERFORM.

       2000-INPUT-CHECK.
           PERFORM until IS-VALID-DECR
             DISPLAY "ENTER INTEGER IN RANGE 00-99"
             ACCEPT NUM-IN-DECR
           END-PERFORM.

