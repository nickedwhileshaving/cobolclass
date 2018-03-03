       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.   HW05NB.                                                  
       AUTHOR.       NIKOLINA BEST.                                                    
       DATE-WRITTEN. FEBRUARY 11 2018.                                           
      *-----------------------------------------------------------------        
      * CORRECT DATA DIVISION TO MATCH INPUT DATA                               
      *-----------------------------------------------------------------        
       ENVIRONMENT DIVISION.                                                    
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  BEGIN-WORKING-STORAGE       PIC X(50)   VALUE                        
            '** DATDIVFL BEGIN WORKING STORAGE **'.                             
                                                                                
       01 WS-DISPLAY-COUNT    PIC 9(2) VALUE 0.                                
       01 CUSTOMER-REC.                                                         
          05 CUST-ID          PIC X(5).                                        
          05 CUST-NAME.                                                         
             10 CUST-TITLE    PIC X(3).                                           
             10 CUST-INIT     PIC X(2).                                           
             10 CUST-SURNAME  PIC X(11).                                        
          05 CUST-GENDER      PIC X(6).                                        
          05 CUST-PAYMENT     PIC 9(6)V9(3).                                     
                                                                                
       01  END-WORKING-STORAGE         PIC X(50)   VALUE                        
            '** DATDEVBL **  END WORKING-STORAGE **'.                           
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
           DISPLAY 'DATDIVFL EXECUTION BEGINS ON '                              
             FUNCTION CURRENT-DATE (1:8) ' AT '                                 
             FUNCTION CURRENT-DATE (9:8).                                       
                                                                                
           PERFORM 2000-INITIALIZE-RECORD.                                      
                                      
           MOVE '75842'   TO CUST-ID.                                           
           MOVE 'MR'      TO CUST-TITLE.                                        
           MOVE 'RD'      TO CUST-INIT.                                         
           MOVE 'FITZROY' TO CUST-SURNAME.                                      
           MOVE 'MALE'    TO CUST-GENDER.                                       
           MOVE 34        TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '82014'   TO CUST-ID.                                           
           MOVE 'MRS'     TO CUST-TITLE                                         
           MOVE 'NM'      TO CUST-INIT.                                         
           MOVE 'BAK'     TO CUST-SURNAME.                                      
           MOVE 'FEMALE'  TO CUST-GENDER.                                       
           MOVE 400045    TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE 'A2214'   TO CUST-ID.                                           
           MOVE 'MRS'     TO CUST-TITLE.                                        
           MOVE 'KA'      TO CUST-INIT.                                         
           MOVE 'RICE'    TO CUST-SURNAME.                                      
           MOVE 'FEMALE'  TO CUST-GENDER.                                       
           MOVE .110    TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '225Z2'    TO CUST-ID.                                           
           MOVE 'MRS'     TO CUST-TITLE.                                        
           MOVE 'OB'      TO CUST-INIT.                                         
           MOVE 'KWIAIT'  TO CUST-SURNAME.                                      
           MOVE 'FEMALE'  TO CUST-GENDER.                                       
           MOVE 2.25      TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '#15R5'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'IM'      TO CUST-INIT.                                         
           MOVE 'WRIGHT'  TO CUST-SURNAME.                                      
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE 7734.34   TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '575T6'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'UR'      TO CUST-INIT.                                         
           MOVE 'WONG  '  TO CUST-SURNAME.                                      
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE 321      TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '78978'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'IO'      TO CUST-INIT.                                         
           MOVE 'SILVER'  TO CUST-SURNAME.                                      
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE .321      TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '2241A'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'OH'      TO CUST-INIT.                                         
           MOVE 'TAKASHAYAWA'  TO CUST-SURNAME.                                 
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE 99.88     TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE 'A1DD3'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'CF'      TO CUST-INIT.                                         
           MOVE 'EYECAIR' TO CUST-SURNAME.                                      
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE 11000.1   TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE 'FG134'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'UB'      TO CUST-INIT.                                         
           MOVE 'WALKEN ' TO CUST-SURNAME.                                      
           MOVE 'MALE  '  TO CUST-GENDER.                                       
           MOVE 8.8       TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE 'FRCDA'   TO CUST-ID.                                           
           MOVE 'MRS'     TO CUST-TITLE.                                        
           MOVE 'IC'      TO CUST-INIT.                                         
           MOVE 'LONDON ' TO CUST-SURNAME.                                      
           MOVE 'FEMALE'  TO CUST-GENDER.                                       
           MOVE 3.157     TO CUST-PAYMENT.                                      
                                                                                
           MOVE 'DF111'   TO CUST-ID.                                           
           MOVE 'MRS'     TO CUST-TITLE.                                        
           MOVE 'IC'      TO CUST-INIT.                                         
           MOVE 'FRANCE ' TO CUST-SURNAME.                                      
           MOVE 'FEMALE'  TO CUST-GENDER.                                       
           MOVE 333       TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           MOVE '56321'   TO CUST-ID.                                           
           MOVE 'MR '     TO CUST-TITLE.                                        
           MOVE 'ES'      TO CUST-INIT.                                         
           MOVE 'KIMOPI' TO CUST-SURNAME.                                      
           MOVE 'MALE'    TO CUST-GENDER.                                       
           MOVE 3.14      TO CUST-PAYMENT.                                      
           PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
                                                                                
           DISPLAY 'DATDIVFL EXECUTION CONCLUDES ON '                           
             FUNCTION CURRENT-DATE (1:8) ' AT '                                 
             FUNCTION CURRENT-DATE (9:8).                                       
           GOBACK.                                                              

       1000-DISPLAY-RECORD-FIELDS.
                                                                                
           ADD 1 TO WS-DISPLAY-COUNT.                                           
           DISPLAY '--'.                                                        
           DISPLAY 'CUSTOMER NUMBER ' WS-DISPLAY-COUNT ':'.                     
           DISPLAY 'CUST-ID:        ' CUST-ID.                                  
           DISPLAY 'CUST-TITLE:     ' CUST-TITLE.                               
           DISPLAY 'CUST-NAME:      ' CUST-NAME.                                
           DISPLAY 'CUST-INIT:      ' CUST-INIT.                                
           DISPLAY 'CUST-SURNAME:   ' CUST-SURNAME.                             
           DISPLAY 'CUST-GENDER:    ' CUST-GENDER.                              
           DISPLAY 'CUST-PAYMENT:   ' CUST-PAYMENT.                             
                                                                                
       2000-INITIALIZE-RECORD.                                                  
           MOVE SPACES TO CUSTOMER-REC.                                         
