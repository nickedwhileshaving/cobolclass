       identification division.
       program-id. HWNB06.
       AUTHOR.     NIKOLINA BEST.
      * ASSIGNMENT 6 - MATH CALCULATIONS
       environment division.
       configuration section.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                   
       data division.
       working-storage section.
       01  WS-NB-NB-WORKING-STORAGE.
      * Add & Modify working storage elements as needed.  Some
      * required elements are coded incorrectly, others are missing.
           05 WS-AVG               PIC 99.99.
           05 WS-AVG-ROUNDED       PIC 9(02).
           05 WS-YEAR              pic 9(04)    value 2016.
           05 WS-YEAR-2            pic 9(02).
           05 WS-YEAR-2-F          pic Z9.
           05 WS-AGE-IN            PIC 9(02)    VALUE 0.
           05 WS-WEIGHT-IN         PIC 9(04).
           05 WS-INCH-HGT-IN       PIC 9(03).
           05 WS-LAST-LEAP-YEAR    PIC 9(04)    VALUE 2016.
           05 WS-NEXT-LEAP-YEAR    PIC 9(04).
           05 WS-NEXT-LEAP-YEAR-F  PIC ZZZ9.
           05 WS-CURRENT-YEAR      PIC 9(04).
           05 WS-YEARS-OLD         PIC 9(02).
           05 WS-BMI               PIC Z9.99.
           05 WS-KILO-IN           PIC 9999V99. 
           05 WS-CENT-IN           PIC 9999V99.
           05 WS-KILO-IN-F         PIC ZZZ9.99. 
           05 WS-CENT-IN-F         PIC ZZZ9.99.
           
           
       01  WS-TIME.
           05 WS-TIME-HH           PIC X(02).
           05 WS-TIME-MM           PIC X(02).
           
       01  WS-TODAYS-DATE.
           05 WS-8-DATE-YEAR.
              10 WS-8-DATE-CC      PIC X(02).
              10 WS-8-DATE-YY      PIC X(02).
           05 WS-8-DATE-MM         PIC X(02).
           05 WS-8-DATE-DD         PIC X(02).

       01 MATH-STUFF. 
           05 FIRST-WHOLE-NUMBER   PIC 9(02).
           05 SECOND-WHOLE-NUMBER  PIC 9(02).
        
       procedure division.
           DISPLAY "START ASGNO6NB FOR NIKOLINA BEST".
           ACCEPT WS-TODAYS-DATE from date yyyymmdd.
           DISPLAY "PROGRAM EXECUTION DATE: " WS-TODAYS-DATE.
           ACCEPT WS-TIME from time.
           DISPLAY "PROGRAM EXECUTION START TIME: " WS-TIME-HH
                   ":" WS-TIME-MM.
      * COMPLETE THE PROCEDURE DIVISION TO PERFORM THESE CALCULATIONS.
      * THIS WILL ALSO INCLUDE THE COMPLETION OF SOME WORKING STORAGE
      * ELEMENTS, AND THE CREATION OF SOME ELEMENTS. 
        
      * CALCULATE THE AVERAGE, UP TO 2 DECIMAL PLACES, OF 
      * TWO 2 DIGIT NUMBERS, ONCE USING THE ROUNDED STATEMENT, 
      * THE 2ND WITHOUT ROUNDING.
           DISPLAY "CALCULATE THE AVG OF 2 WHOLE NUMBERS". 
           ACCEPT FIRST-WHOLE-NUMBER.
           ACCEPT SECOND-WHOLE-NUMBER.
           COMPUTE WS-AVG = (FIRST-WHOLE-NUMBER + 
                   SECOND-WHOLE-NUMBER) / 2.
           DISPLAY "The Average of " FIRST-WHOLE-NUMBER " and "
                   SECOND-WHOLE-NUMBER " is " WS-AVG.
           COMPUTE WS-AVG-ROUNDED ROUNDED = (FIRST-WHOLE-NUMBER + 
                   SECOND-WHOLE-NUMBER) / 2.
           DISPLAY "The Rounded Average of " FIRST-WHOLE-NUMBER " and "
                   SECOND-WHOLE-NUMBER " is " WS-AVG-ROUNDED.

      * 2016 WAS A LEAP YEAR. CALCULATE AND DISPLAY EACH OF THE 
      * NEXT 4 LEAP YEARS.
           COMPUTE WS-YEAR = WS-LAST-LEAP-YEAR + 4.
           DISPLAY "NEXT LEAP YEAR: " WS-YEAR.
           COMPUTE WS-YEAR = WS-YEAR + 4.
           DISPLAY "2ND LEAP YEAR FROM NOW: " WS-YEAR.
           COMPUTE WS-YEAR = WS-YEAR + 4.
           DISPLAY "3RD LEAP YEAR FROM NOW: " WS-YEAR.
           COMPUTE WS-YEAR = WS-YEAR + 4.
           DISPLAY "4TH LEAP YEAR FROM NOW: " WS-YEAR.
           
      * DISPLAY FOR THE USER HOW MANY YEARS UNTIL THE NEXT LEAP YEAR
           MOVE WS-8-DATE-YEAR TO WS-CURRENT-YEAR.
           COMPUTE WS-NEXT-LEAP-YEAR = 2020 - WS-CURRENT-YEAR.
           MOVE WS-NEXT-LEAP-YEAR TO WS-NEXT-LEAP-YEAR-F. 
           DISPLAY "THIS IS THE NUMBER OF YEARS UNTIL THE NEXT LEAP"
                   " YEAR " WS-NEXT-LEAP-YEAR-F.

      * ASK THE USER FOR THEIR AGE.  DISPLAY HOW OLD THEY
      * WILL BE FOR THE NEXT 2 LEAP YEARS.
           DISPLAY "WHAT IS YOUR AGE?".
           ACCEPT WS-AGE-IN.
           COMPUTE WS-YEARS-OLD = WS-AGE-IN + WS-NEXT-LEAP-YEAR.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " ON NEXT LEAP YEAR".
           COMPUTE WS-YEARS-OLD = WS-YEARS-OLD + 4.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " IN 2 LEAP YEARS".
           
      * DISPLAY FOR THE USER HOW MANY YEARS UNTIL THEY TURN 100
           COMPUTE WS-YEAR-2 = 100 - WS-AGE-IN.
           MOVE WS-YEAR-2 TO WS-YEAR-2-F.
           DISPLAY "YEARS UNTIL YOU TURN 100: " WS-YEAR-2-F.

      * PROMPT THE USE FOR THEIR WEIGHT IN POUNDS AND HEIGHT IN INCHES.
      * CALCULATE AND DISPLAY THE USERS BMI TO TWO DECIMAL PLACES.
      * LOOK UP FORMULA ON WEB AND USE COBOL COMPUTE STATEMENT TO 
      * TO CALCULATE THE BMI.
           DISPLAY "PLEASE ENTER YOUR WEIGHT IN POUNDS".
           ACCEPT WS-WEIGHT-IN.
           DISPLAY "PLEASE ENTER YOUR HEIGHT IN INCHES".
           ACCEPT WS-INCH-HGT-IN.
           COMPUTE WS-BMI = 703 * (WS-WEIGHT-IN /
                                   (WS-INCH-HGT-IN ** 2)).
           DISPLAY "YOUR BMI IS: " WS-BMI. 
           
      * FOR 5 EXTRA POINTS, CONVERT THE INPUT WEIGHT & HEIGHT TO 
      * METRIC MEASUREMENTS, DISPLAY THEM AND METRIC BMI.
           COMPUTE WS-KILO-IN = (WS-WEIGHT-IN * .45359237)
           MOVE WS-KILO-IN TO WS-KILO-IN-F.
           DISPLAY "YOUR WEIGHT IN KILOGRAMS " WS-KILO-IN-F.
           COMPUTE WS-CENT-IN = (WS-INCH-HGT-IN * 2.54)
           MOVE WS-CENT-IN TO WS-CENT-IN-F.
           DISPLAY "YOUR HEIGHT IN CENTIMETERS " WS-CENT-IN-F.
           COMPUTE WS-BMI = WS-KILO-IN / ((WS-CENT-IN / 100) ** 2).
           DISPLAY "YOUR BMI IS: " WS-BMI. 
           DISPLAY "END OF ASSIGNMENT 06".
           GOBACK.
