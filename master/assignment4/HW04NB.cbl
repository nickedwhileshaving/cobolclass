       identification division.
       program-id. HW04NB.
       author. Nikolina Best.
       date-written. 01/30/2018.
      *Using condition names (level 88's) and the EVALUATE

       data division.
       working-storage section.
       01 CharIn PIC X.
           88 Vowel          VALUE "a", "e", "i", "o", "u".
           88 Consonant      VALUE "b", "c", "d", "f", "g", "h"
                           "j" THRU "n", "p" THRU "t", "v" THRU "z".
           88 Digit          VALUE "0" THRU "9".
           88 ValidCharacter VALUE "a" THRU "z", "0" THRU "9".

       procedure division.
       begin.
       DISPLAY "Enter lower case character or digit. Invalid char ends."
           ACCEPT CharIn
           PERFORM UNTIL NOT ValidCharacter
               EVALUATE TRUE
                   WHEN Vowel
                       DISPLAY "The letter " CharIn " is a vowel."
                   WHEN Consonant
                       DISPLAY "The letter " CharIn " is a consonant."
                   WHEN Digit
                       DISPLAY CharIn " is a digit."
               END-EVALUATE
               ACCEPT CharIn
           END-PERFORM
           Stop Run.
           
       end program HW04NB.