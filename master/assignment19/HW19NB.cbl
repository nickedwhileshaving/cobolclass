       IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    HW19NB.
000300 AUTHOR.        NIKOLINA BEST.
000400 DATE-WRITTEN.  05/05/2018.
000500 INSTALLATION.  INFSYS-3807 COBOL.

003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 INPUT-OUTPUT SECTION.
004000 FILE-CONTROL.
           SELECT WS-PRESIDENTS
001300     assign to
           "C:\school\cobol\cobolclass\master\data\USPRES.dat"
           organization is line sequential
           FILE STATUS IS WS-PRESIDENTS-STATUS.
004600
004700 DATA DIVISION.
004800
004900 FILE SECTION.
005000
005100 FD  WS-PRESIDENTS
005200     RECORDING MODE IS F
005300     DATA RECORD IS WS-PRESIDENTS-IN.
005400
005500 01 WS-PRESIDENTS-IN.
005600    05  WS-PRESIDENTS-RECORD     PIC X(27).


007600 WORKING-STORAGE SECTION.
009100 01 WS-PRESIDENTS-STATUS       PIC X(02) VALUE "00".
       01 WS-PRESIDENT-FILE.
          05  WS-PRESIDENT-RECORD OCCURS 45 TIMES.
              10  WS-PRESIDENTS-INDEX      PIC X(02).
              10  WS-PRESIDENTS-NAME       PIC X(25).

       01 WS-COUNTERS.
          05 WS-PRESIDENT-SUB          PIC 9(02).
             88 WS-VALID-PRESIDENT-SUB VALUE 1 THRU 45.

       01 WS-MISC.
          05 WS-THE-PROMPT             PIC X(46) 
                  VALUE "WHICH PRESIDENT DO YOU WANT TO KNOW (01 - 45)?".

       01 CharIn PIC X(02).
           88 ExitCharacter VALUE "x", "X".
           88 ListAll       VALUE "LA", "la".



009400 PROCEDURE DIVISION.
010800
010900     DISPLAY "HW19NB".
           DISPLAY "BY: NIKOLINA BEST".
           PERFORM 1000-INITIALIZE.
           DISPLAY WS-THE-PROMPT.
           ACCEPT CharIn.
           PERFORM UNTIL ExitCharacter
               IF ListAll
                   PERFORM 5000-LIST-ALL
               ELSE
                   PERFORM 2000-LOOKUP-DISPLAY-CHOSEN-PRESIDENT
               END-IF
               DISPLAY WS-THE-PROMPT
               ACCEPT CharIn
           END-PERFORM.
           STOP-RUN.
      *    PERFORM 5000-LIST-ALL.
011000 
015600     GOBACK.

       1000-INITIALIZE.
           OPEN INPUT  WS-PRESIDENTS.
           READ WS-PRESIDENTS.
      *    FILL IN OUR TABLE.
           MOVE 1 TO WS-PRESIDENT-SUB.
011800     PERFORM UNTIL WS-PRESIDENTS-STATUS > "00"
               MOVE WS-PRESIDENTS-RECORD TO WS-PRESIDENT-RECORD(WS-PRESIDENT-SUB)
               ADD 1 TO WS-PRESIDENT-SUB
013900       READ WS-PRESIDENTS
014200     END-PERFORM.
           CLOSE WS-PRESIDENTS.

       2000-LOOKUP-DISPLAY-CHOSEN-PRESIDENT.
           MOVE CharIn TO WS-PRESIDENT-SUB.
           IF WS-VALID-PRESIDENT-SUB
               DISPLAY "THE PRESIDENT'S POSITION: " WS-PRESIDENT-SUB
               DISPLAY "THE PRESIDENT'S NAME: "
                   WS-PRESIDENTS-NAME (WS-PRESIDENT-SUB)
           ELSE
               DISPLAY "The president was not found."
           END-IF.

       5000-LIST-ALL.
           PERFORM VARYING WS-PRESIDENT-SUB FROM 1 BY 1
             UNTIL WS-PRESIDENT-SUB = 45 + 1
               DISPLAY "THE PRESIDENT'S POSITION: " WS-PRESIDENT-SUB
               DISPLAY "THE PRESIDENT'S NAME: "
                   WS-PRESIDENTS-NAME (WS-PRESIDENT-SUB)
               DISPLAY ""
           END-PERFORM.


