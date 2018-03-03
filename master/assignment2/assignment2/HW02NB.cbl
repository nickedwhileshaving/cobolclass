       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW02NB
      *Program to display cobol greetings.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ITER-NUM PIC 9 VALUE 5.
       PROCEDURE DIVISION.
       BEGIN.
           PERFORM DISPLAY-GREETING ITER-NUM TIMES.
           STOP-RUN.
       DISPLAY-GREETING.
           DISPLAY "COBOL greeting from Niki Best".
       stop run. 
       END PROGRAM HW02NB.
       