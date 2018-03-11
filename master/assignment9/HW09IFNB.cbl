       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW09IFNB.
      * ASK USED FOR ROCK, PAPER OR SCISSORS.
      * COMPARE THEIR INPUT ANSWER TO THE RANDOM NUMBER
      * GENERATED BY THE PROGRAM.  PROVIDE WINNER/LOSER FEEDBACK. 
       AUTHOR. NIKOLINA BEST.
       DATE-WRITTEN. 03/11/18.

       data division.
       working-storage section.
       01  WS-TODAYS-DATE          pic x(08).
       01  WS-TIME.
           05 WS-TIME-HH           pic 9(02).
           05 WS-TIME-MM           PIC 9(02).
           05 WS-TIME-SS           PIC 9(02).
           05 WS-TIME-MS.
              10 WS-TIME-MS1       PIC 9(01).
              10 WS-TIME-MS2       PIC 9(01).
                 88 RANDOM-ROCK              VALUE 0 1 2.     
                 88 RANDOM-PAPER             VALUE 3 4 5.
                 88 RANDOM-SCISSORS          VALUE 6 7 8.
                 88 VALID-RANDOM-DIGIT       VALUE 0 THRU 8.
                    
       01 WS-CHOICE                PIC X(01).
          88 ROCK-CHOSEN                     VALUE 'R' 'r'.
          88 PAPER-CHOSEN                    VALUE 'P' 'p'.
          88 SCISSORS-CHOSEN                 VALUE 'S' 's'.
          88 VALID-CHOICE                    VALUE 'R' 'r'
                                                   'P' 'p'
                                                   'S' 's'
                                                   'X' 'x'
                                                   'Q' 'q'.
          88 EXIT-CHOICE                     VALUE 'X' 'x'
                                                   'Q' 'q'.
       PROCEDURE DIVISION.
       0000-MAINLINE.
           DISPLAY 'START RPSGAMFL BY NIKOLINA BEST'.
           ACCEPT WS-TODAYS-DATE from date yyyymmdd.
           DISPLAY "PROGRAM EXECUTION DATE      : " WS-TODAYS-DATE.
           ACCEPT WS-TIME from time.
           DISPLAY "PROGRAM EXECUTION START TIME: " WS-TIME.
           DISPLAY "R FOR ROCK, P FOR PAPER, S FOR SCISSORS"
           DISPLAY "ENTER X OR Q TO EXIT"
        
      * PROMPT THE USER FOR GAME CHOICE UNTIL A VALID CHOICE IS MADE. 
           PERFORM UNTIL EXIT-CHOICE
               PERFORM 1000-SET-RANDOM-NUMBER
               ACCEPT WS-CHOICE
               PERFORM 2000-DISPLAY-RANDOM-SELECTION
               PERFORM 3000-SHOW-USER-SELECTION
               PERFORM 4000-DETERMINE-WINNER
           END-PERFORM.
           GOBACK.

       1000-SET-RANDOM-NUMBER.
      * QUASI-RANDOM NUMBER GENERATOR FOR GAME    
           MOVE 9 TO WS-TIME-MS2.
           PERFORM UNTIL VALID-RANDOM-DIGIT
             ACCEPT WS-TIME from time
           END-PERFORM.

       2000-DISPLAY-RANDOM-SELECTION.
           IF RANDOM-ROCK then
               DISPLAY "COMPUTER CHOSE ROCK"
           ELSE 
               IF RANDOM-PAPER then
                   DISPLAY "COMPUTER CHOSE PAPER"
               ELSE 
                   DISPLAY "COMPUTER CHOSE SCISSORS"
               END-IF
           END-IF.

       3000-SHOW-USER-SELECTION.
           IF ROCK-CHOSEN then
               DISPLAY "USER CHOSE ROCK"
           ELSE 
               IF PAPER-CHOSEN then
                   DISPLAY "USER CHOSE PAPER"
               ELSE 
                   IF SCISSORS-CHOSEN THEN
                   DISPLAY "USER CHOSE SCISSORS"
                   ELSE 
                       IF EXIT-CHOICE THEN
                       DISPLAY "YOU HAVE OPTED OUT"
                           GOBACK
                       END-IF
                   END-IF
               END-IF
           END-IF.

       4000-DETERMINE-WINNER.
           IF VALID-CHOICE THEN
               IF RANDOM-ROCK THEN
                   IF ROCK-CHOSEN then
                       DISPLAY "YOU HAVE TIED"
                   ELSE 
                       IF PAPER-CHOSEN THEN
                           DISPLAY "PAPER COVERS ROCK - PAPER WINS "
                             "ROCK LOSES"
                           DISPLAY "YOU WIN!"
                       ELSE 
                           DISPLAY "ROCK CRUSHES SCISSORS - ROCK WINS, "
                             "SCISSORS LOSES"
                           DISPLAY "YOU LOSE!"
                       END-IF
               ELSE 
                   IF RANDOM-PAPER THEN
                       IF PAPER-CHOSEN then
                           DISPLAY "YOU HAVE TIED"
                       else
                           IF SCISSORS-CHOSEN then
                               DISPLAY "SCISSOR CUTS PAPER - SCISSORS WIN"
                               " PAPER LOSES"
                               DISPLAY "YOU WIN!"
                           else
                               DISPLAY "PAPER COVERS ROCK - PAPER WINS"
                               " ROCK LOSES"
                               DISPLAY "YOU LOSE"
                           END-IF
                     END-IF
                 ELSE
                   IF RANDOM-SCISSORS then
                       IF SCISSORS-CHOSEN then
                           DISPLAY "YOU HAVE TIED"
                       ELSE 
                           IF ROCK-CHOSEN THEN 
                               DISPLAY "ROCK CRUSHES SCISSORS - ROCK WINS"
                               " SCISSOR LOSES"
                               DISPLAY "YOU WIN!"
                           ELSE 
                               DISPLAY "SCISSOR CUTS PAPER - SCISSOR WINS"
                               " PAPER LOSES"
                               DISPLAY "YOU LOSE!"
                           END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE 
               DISPLAY "PLEASE ENTER A VALID CHOICE"
           END-IF