       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW04NB.
       AUTHOR. NIKOLINA BEST.
      *Top Ten Subcompact Crossovers.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-TYPE-OF-CAR       PIC X(10).
           88 WS-DOMESTIC      VALUE "Encore", "Renegade", "Ecosport",
                               "Trax".
           88 WS-FOREIGN       VALUE "HRV", "500X", "CX7", "X1",
                               "Countryman", "Juke". 
           88 WS-1000-LBS      VALUE "Encore", "EcoSport", "Trax",
                               "Renegade", "X1".
           88 WS-2000-LBS      VALUE "Renegade", "X1".

       PROCEDURE DIVISION.

       BEGIN.
           DISPLAY "Choose a Top 10 Subcompact Crossover from the list"
               ": ".

           PERFORM 1000-LIST-MODELS.

           ACCEPT WS-TYPE-OF-CAR.

           IF WS-1000-LBS THEN
               IF WS-DOMESTIC THEN
                   DISPLAY WS-TYPE-OF-CAR " Top 10 Domestic Model with"
                   " over 1000 lbs towing capacity."
               ELSE
                   IF WS-FOREIGN THEN
                       DISPLAY WS-TYPE-OF-CAR " is a Top 10 Import " 
                       "Model with over 1000 lbs towing capacity."
                   ELSE
                       DISPLAY "Your Choice is not a top 10 Model."
                   END-IF
               END-IF
           ELSE 
               IF WS-DOMESTIC THEN
                   DISPLAY WS-TYPE-OF-CAR " Top 10 Domestic Model with"
                   " no towing capacity."
               ELSE
                   IF WS-FOREIGN THEN
                       DISPLAY WS-TYPE-OF-CAR " is a Top 10 Import " 
                       "Model with no towing capacity."
                   ELSE
                       DISPLAY "Your Choice is not a top 10 Model."
                   END-IF
               END-IF
           END-IF

           STOP RUN.

    
       1000-LIST-MODELS.                                                  
               DISPLAY "Encore".
               DISPLAY "Renegade".
               DISPLAY "Ecosport".
               DISPLAY "HRV".
               DISPLAY "500X".
               DISPLAY "CX7".
               DISPLAY "X1".
               DISPLAY "Countryman".
               DISPLAY "Juke".
               DISPLAY "Trax".

       