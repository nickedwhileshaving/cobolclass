       identification division.
       program-id. "HW03NB".
       author.      Nikolina Best.

       data division.                                                   
       working-storage section.                                        
       01 CalcResult   PIC 9. 
       01 FirstNum     PIC 9 Value 2.
       01 SecondNum    PIC 9 Value 3.

       procedure division.
           compute CalcResult = FirstNum * SecondNum.
           display "Multiplication Result is: " CalcResult.
           display FirstNum.
           display SecondNum.
           goback.
       end program HW03NB.