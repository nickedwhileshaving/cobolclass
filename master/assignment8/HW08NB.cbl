       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW08NB.
       AUTHOR. Nikolina Best.
       DATE-WRITTEN. 02/25/18.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ZIP-INPUT PIC X(02).
           88 ZIP-ST-LOUIS-AREA        VALUE "63".
           88 ZIP-KANSAS-CITY-AREA     VALUE "64".
           88 ZIP-OUT-STATE-MISSOURI   VALUE "65".
           88 ZIP-EAST-ST-LOUIS        VALUE "62".
           88 ZIP-OUT-STATE-ILLINOIS   VALUE "61".
           88 ZIP-CHICAGO-AREA         VALUE "60".
           88 OUTSIDE-AREA             VALUE "00" THRU "59", "66" THRU
                                             "99".

       01 ENTRY-VALUE                  PIC X(05).
       01 F-DATA                       PIC X(05).
       01 A-COUNTER                    PIC 9(01).
       01 TEMP-COUNTER                 PIC 9(01).

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter a five digit zip code - " WITH NO ADVANCING

           ACCEPT ENTRY-VALUE.

           string ENTRY-VALUE, "." delimited by " " into F-DATA.

           Inspect F-DATA TALLYING A-COUNTER for characters
              before initial ".".

           IF A-COUNTER < 5 THEN
               DISPLAY "The number entered does not contain 5 digits."
                   WITH NO ADVANCING
           ELSE 
               PERFORM 2000-IS-NUMERIC
               DISPLAY TEMP-COUNTER
               IF TEMP-COUNTER < 5 then
                   DISPLAY "IT IS NOT NUMERIC."
               ELSE 
                   DISPLAY "IT IS NUMERIC."
                   PERFORM 3000-CHECK-AREA
                   PERFORM 4000-DO-SHIPPING
               END-IF
           END-IF.

           MOVE ENTRY-VALUE(1:2) TO ZIP-INPUT.

       STOP RUN.

       2000-IS-NUMERIC.
           INSPECT ENTRY-VALUE TALLYING
           TEMP-COUNTER FOR ALL "0", ALL "1", ALL "2", ALL "3",
               ALL "4", ALL "5", ALL "6", ALL "7", ALL "8", 
               ALL "9".

       3000-CHECK-AREA.
           MOVE ENTRY-VALUE(1:2) TO ZIP-INPUT
           IF ZIP-ST-LOUIS-AREA then
               DISPLAY "This is St. Louis"
           END-IF
           IF ZIP-KANSAS-CITY-AREA
               DISPLAY "This is Kansas City"
           END-IF
           IF ZIP-EAST-ST-LOUIS
               DISPLAY "This is East St. Louis"
           END-IF
           IF ZIP-CHICAGO-AREA
               DISPLAY "This is Chicago"
           END-IF
           IF ZIP-OUT-STATE-MISSOURI
               DISPLAY "This is Out State MO"
           END-IF
           IF ZIP-OUT-STATE-ILLINOIS
               DISPLAY "This is Out State IL"
           END-IF
           IF OUTSIDE-AREA THEN
               DISPLAY "Outside Bi-State Region"
           END-IF.

       4000-DO-SHIPPING.
           IF ZIP-ST-LOUIS-AREA OR ZIP-KANSAS-CITY-AREA or
             ZIP-CHICAGO-AREA OR ZIP-EAST-ST-LOUIS then
               DISPLAY "St. Louis, Kansas City & Chicago are two day"
               " shipping."
           END-IF
           IF ZIP-OUT-STATE-ILLINOIS OR ZIP-OUT-STATE-MISSOURI then
               DISPLAY "Out State Illinois and Missouri are three day"
               " shipping."
           END-IF
           IF OUTSIDE-AREA then
               DISPLAY "If it is not MO or IL, the shipping is four"
               " days."
           END-IF