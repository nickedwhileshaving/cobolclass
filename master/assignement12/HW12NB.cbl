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
              NUM-IN               PIC 99.
           
       PROCEDURE DIVISION.
           DISPLAY "PLEASE ENTER A STARTING NUMBER OF BOTTLES"
           PERFORM 1110-INPUT-CHECK.
           PERFORM VARYING BOTTLES FROM NUM-IN-INT BY -1 UNTIL
               BOTTLES = -1
               DISPLAY SPACES
               SUBTRACT 1 FROM BOTTLES GIVING REMAINING-BOTTLES
               EVALUATE BOTTLES
                   WHEN 0
                       DISPLAY "NO MORE BOTTLES OF BEER ON THE WALL, "
                               "NO MORE BOTTLES OF BEER..."
                       DISPLAY "GET THE HAT AND PASS IT AROUND, "
                               "TIME TO B DOUBLE E DOUBLE R U-N, "
                               "BEER RUN."
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
                       DISPLAY "TAKE ONE DOWN, PASS IT AROUND, "
                               REMAINING-BOTTLES
                               (STARTING-POSITION:POSITIONS)
                               " BOTTLES OF BEER ON THE WALL."
               END-EVALUATE
           END-PERFORM
           ACCEPT POSITIONS.
           STOP RUN.

       1110-INPUT-CHECK.
           PERFORM until IS-VALID
             DISPLAY "ENTER INTEGER IN RANGE 00-99"
             ACCEPT NUM-IN
           END-PERFORM.

