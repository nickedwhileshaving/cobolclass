       IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    HW16NB.
000300 AUTHOR.        NIKOLINA BEST.
000400 DATE-WRITTEN.  04/29/2018.
000500 INSTALLATION.  INFSYS-3807 COBOL.

003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 INPUT-OUTPUT SECTION.
004000 FILE-CONTROL.
           SELECT PAYROLL-IN
001300     assign to
           "C:\school\cobol\cobolclass\master\data\HW16IN01.dat"
           organization is line sequential
           FILE STATUS IS WS-PAYROLL-IN-STATUS.

004400     SELECT PAYROLL-OUT
           assign to
           "C:\school\cobol\cobolclass\master\data\HW16OU01.dat"
           ORGANIZATION IS LINE SEQUENTIAL
004500       FILE STATUS IS WS-PAYROLL-FILE-OUT-STATUS.

004510     SELECT PAYROLL-RPT  assign to
           "C:\school\cobol\cobolclass\master\data\HW16OU02.dat" 
           ORGANIZATION IS LINE SEQUENTIAL
004520       FILE STATUS IS WS-PAYROLL-RPT-STATUS.
004600
004700 DATA DIVISION.
004800
004900 FILE SECTION.
005000
005100 FD  PAYROLL-IN
005200     RECORDING MODE IS F
005300     DATA RECORD IS PAYROLL-REC-IN.
005400
005500 01 PAYROLL-REC-IN.
005600    05  PAYROLL-PAY-DATE         PIC X(06).
005700    05  PAYROLL-EMP-NUMBER       PIC X(03).
005800    05  PAYROLL-EMP-HOURS        PIC 9(02)V99.
005900    05  PAYROLL-EMP-RATE         PIC 9(02)V99.
006000    05  PAYROLL-EMP-DEDUCTIONS   PIC X(01).
          05  FILLER                   PIC X(03).

006100
006200 FD  PAYROLL-OUT
006300     RECORDING MODE IS F
006400     DATA RECORD IS PAYROLL-REC-OUT.
006500
006600 01  PAYROLL-REC-OUT.
006700     05  PAYROLL-EMP-OUT          PIC X(21).
006800     05  PAYROLL-OUT-CALCULATED.
006900         15  PAYROLL-EMP-GROSS    PIC 9(04)V99.
007000         15  PAYROLL-EMP-SSN-MED  PIC 9(03)V99.
007100         15  PAYROLL-EMP-STATE    PIC 9(03)V99.
007200         15  PAYROLL-EMP-LOCAL    PIC 9(03)V99.
007300         15  PAYROLL-EMP-FED      PIC 9(03)V99.
007400         15  PAYROLL-EMP-NET      PIC 9(04)V99.
007500

       FD  PAYROLL-RPT
003000     RECORDING MODE IS F
003100     DATA RECORD IS SHAPE-REC-OUT-RPT.
003200 01 PAYROLL-REC-OUT-RPT         PIC X(133).
       
007600 WORKING-STORAGE SECTION.
007700 01 WS-TOTAL-FIELDS.
          05 WS-TOTAL-HOURS             PIC 9(06).
          05 WS-TOTAL-OVERTIME          PIC 9(07)V99.
007800    05 WS-TOTAL-GROSS             PIC 9(07)V99.
007900    05 WS-TOTAL-NET               PIC 9(07)V99.
008000    05 WS-TOTAL-STATE-TAX         PIC 9(07)V99.
008100    05 WS-TOTAL-LOCAL-TAX         PIC 9(07)V99.
008200    05 WS-TOTAL-FED-TAX           PIC 9(07)V99.
008300    05 WS-TOTAL-MED-SS-WH         PIC 9(07)V99.
008400    05 WS-TOTAL-DEDUCTIONS        PIC 9(07)V99.
          05 WS-GRAND-TOTAL-DEDUCT      PIC 9(07)V99.
008500
008600 01 WS-FORMATTED-OUTPUT.
008700    05 WS-FORMATTED-GROSS         PIC $Z,ZZZ,ZZZ.99.
008800    05 WS-FORMATTED-NET           PIC $Z,ZZZ,ZZZ.99.
008900    05 WS-FORMATTED-WH            PIC $Z,ZZZ,ZZZ.99.
009000
009100 01 WS-PAYROLL-IN-STATUS       PIC X(02) VALUE "00".
009200 01 WS-PAYROLL-FILE-OUT-STATUS PIC X(02) VALUE "00".
       01 WS-PAYROLL-RPT-STATUS      PIC X(02) VALUE "00".
       01 WS-DATE.
          05 WS-DATE-MM              PIC X(02).
          05 FILLER                  PIC X VALUE "/".
          05 WS-DATE-DD              PIC X(02).
          05 FILLER                  PIC X VALUE "/".
          05 WS-DATE-YY              PIC X(02).
009300
009310* -YOU WILL NEED TO ADD THE REPORT HEADERS HERE AND WRITE
009320*  THEM AT THE BEGINNING OF THE PROGRAM BEFORE ANY REPORT
009330*  DETAIL LINES CONTAINING PAYROLL AMOUNTS ARE WRITE.
       01 WS-PAYROLL-RPT-HDR1.
          05 FILLER                  PIC X(60) VALUE ALL SPACES.
          05 FILLER                  PIC X(14) VALUE "PAYROLL REPORT".
          05 FILLER                  PIC X(59) VALUE ALL SPACES.
       01 WS-PAYROLL-RPT-HDR2.
          05 FILLER                  PIC X(66) VALUE ALL SPACES.
          05 FILLER                  PIC X(02) VALUE "BY".
          05 FILLER                  PIC X(65) VALUE ALL SPACES.
       01 WS-PAYROLL-RPT-HDR3.
          05 FILLER                  PIC X(61) VALUE ALL SPACES.
          05 FILLER                  PIC X(13) VALUE "NIKOLINA BEST".
          05 FILLER                  PIC X(59) VALUE ALL SPACES.
       01 WS-PAYROLL-RPT-HDR4.
          05 FILLER                  PIC X(60) VALUE ALL SPACES.
          05 FILLER                  PIC X(06) VALUE "AS OF ".
          05 WS-PAYROLL-RPT-DATE     PIC X(08).
          05 FILLER                  PIC X(57) VALUE ALL SPACES.
       01 WS-PAYROLL-RPT-HDR5.
          05 FILLER                  PIC X(21) VALUE ALL SPACES.
          05 FILLER                  PIC X(07) VALUE "PAYDATE".
          05 FILLER                  PIC X(04) VALUE ALL SPACES.
          05 FILLER                  PIC X(08) VALUE "EMP. NUM".
          05 FILLER                  PIC X(01) VALUE ALL SPACES.
          05 FILLER                  PIC X(12) VALUE "HOURS WORKED".
          05 FILLER                  PIC X(02) VALUE ALL SPACES.
          05 FILLER                  PIC X(11) VALUE "HOURLY RATE".
          05 FILLER                  PIC X(01) VALUE ALL SPACES.
          05 FILLER                  PIC X(08) VALUE "BASE PAY".
          05 FILLER                  PIC X(02) VALUE ALL SPACES.
          05 FILLER                  PIC X(08) VALUE "OVERTIME".
          05 FILLER                  PIC X(03) VALUE ALL SPACES.
          05 FILLER                  PIC X(05) VALUE "GROSS".
          05 FILLER                  PIC X(04) VALUE ALL SPACES.
          05 FILLER                  PIC X(06) VALUE "DEDUCT".
          05 FILLER                  PIC X(05) VALUE ALL SPACES.
          05 FILLER                  PIC X(03) VALUE "NET".
       01 WS-PAYROLL-RPT-DETAIL.
          05 FILLER                  PIC X(21) VALUE ALL SPACES.
          05 DTL-DATE.
             06 DTL-DATE-MM          PIC X(02).
             06 FILLER               PIC X VALUE "/".
             06 DTL-DATE-DD          PIC X(02).
             06 FILLER               PIC X VALUE "/".
             06 DTL-DATE-YY          PIC X(02).
          05 FILLER                  PIC X(04) VALUE ALL SPACES.
          05 DTL-EMP-OUT             PIC X(06).
          05 FILLER                  PIC X(07) VALUE ALL SPACES.
          05 DTL-HOURS-WORKED        PIC 9(02).
          05 FILLER                  PIC X(10) VALUE ALL SPACES.
          05 DTL-HOURLY-RATE         PIC $$.$$.
          05 FILLER                  PIC X(04) VALUE ALL SPACES.
          05 DTL-BASE-PAY            PIC $$$$.$$.
          05 FILLER                  PIC X(03) VALUE ALL SPACES.
          05 DTL-OVERTIME            PIC $$$9.99.
          05 FILLER                  PIC X(03) VALUE ALL SPACES.
          05 DTL-GROSS               PIC $$$$.$$.
          05 FILLER                  PIC X(03) VALUE ALL SPACES.
          05 DTL-DEDUCT              PIC $$$$.$$.
          05 FILLER                  PIC X(03) VALUE ALL SPACES.
          05 DTL-NET                 PIC $$$$.$$.
       01 WS-PAYROLL-RPT-DASH-LINE.
          05 FILLER                  PIC X(21) VALUE ALL SPACES.
          05 FILLER                  PIC X(93) VALUE ALL "-".
       01 WS-PAYROLL-RPT-TOTAL-LINE.
          05 FILLER                  PIC X(21) VALUE ALL SPACES.
          05 TOTAL-PAYDATE           PIC X(08).
          05 FILLER                  PIC X(01) VALUE ALL SPACES.
          05 FILLER                  PIC X(06) VALUE "TOTALS".
          05 FILLER                  PIC X(08) VALUE ALL SPACES.
          05 TOTAL-HOURS-WORKED      PIC ZZZ9.
          05 FILLER                  PIC X(28) VALUE ALL SPACES.
          05 TOTAL-OVERTIME          PIC $$$$$.$$.
          05 FILLER                  PIC X(01) VALUE ALL SPACES.
          05 TOTAL-GROSS             PIC $$$$$$.$$.
          05 FILLER                  PIC X(02) VALUE ALL SPACES.
          05 TOTAL-DEDUCT            PIC $$$$$.$$.
          05 FILLER                  PIC X(01) VALUE ALL SPACES.
          05 TOTAL-NET-RPT           PIC $$$$$$.$$.

009400 PROCEDURE DIVISION.
009900*    PERFORM LOOP TO:
010000*     -DO CALCULATIONS
010100*     -UPDATE TOTALS
010200*     -WRITE OUT NEW RECORD
010300*     -WRITE REPORT RECORD (==> YOU ADD THIS)
010400*     -READ NEXT RECORD
010500*    WRITE REPORT SUBTOTALS (==> YOU ADD THIS)
010600*    DISPLAY PROGRAM TOTALS
010700*    CLOSE FILES
010800
010900     DISPLAY "HW16NB BEGINS".
           PERFORM 1000-INITIALIZE.
011000 
011500     READ PAYROLL-IN.
011600     DISPLAY "WS-PAYROLL-IN-STATUS AFTER READ 1: "
011700              WS-PAYROLL-IN-STATUS.
011800     PERFORM UNTIL WS-PAYROLL-IN-STATUS > "00"
011900                OR WS-PAYROLL-FILE-OUT-STATUS > "00"
012000       MOVE PAYROLL-REC-IN TO PAYROLL-EMP-OUT
012100       MOVE ZEROS          TO PAYROLL-OUT-CALCULATED
012210       COMPUTE PAYROLL-EMP-GROSS   = PAYROLL-EMP-HOURS
012300                                   * PAYROLL-EMP-RATE
012400       COMPUTE PAYROLL-EMP-STATE   = PAYROLL-EMP-GROSS * .06
012500       COMPUTE PAYROLL-EMP-SSN-MED = PAYROLL-EMP-GROSS * .0765
012600       COMPUTE PAYROLL-EMP-LOCAL   = PAYROLL-EMP-GROSS * .01
012700       COMPUTE PAYROLL-EMP-FED     = PAYROLL-EMP-GROSS * .20
012800       COMPUTE PAYROLL-EMP-NET     = PAYROLL-EMP-GROSS
012900                                   - PAYROLL-EMP-STATE
013000                                   - PAYROLL-EMP-SSN-MED
013100                                   - PAYROLL-EMP-LOCAL
013200                                   - PAYROLL-EMP-FED
013500       WRITE PAYROLL-REC-OUT
             MOVE PAYROLL-PAY-DATE(1:2) TO DTL-DATE-MM
             MOVE PAYROLL-PAY-DATE(3:4) TO DTL-DATE-DD
             MOVE PAYROLL-PAY-DATE(5:6) TO DTL-DATE-YY
             MOVE PAYROLL-EMP-NUMBER TO DTL-EMP-OUT
             MOVE PAYROLL-EMP-HOURS TO DTL-HOURS-WORKED
             MOVE PAYROLL-EMP-RATE TO DTL-HOURLY-RATE
             COMPUTE WS-TOTAL-DEDUCTIONS = PAYROLL-EMP-GROSS
014400                                 - PAYROLL-EMP-NET
014500       MOVE PAYROLL-EMP-GROSS      TO WS-FORMATTED-GROSS
014600       MOVE WS-TOTAL-DEDUCTIONS TO WS-FORMATTED-WH
014700       MOVE PAYROLL-EMP-NET        TO WS-FORMATTED-NET
             MOVE PAYROLL-EMP-GROSS      TO DTL-BASE-PAY
             MOVE PAYROLL-EMP-GROSS      TO DTL-GROSS
             MOVE WS-TOTAL-DEDUCTIONS    TO DTL-DEDUCT
             MOVE PAYROLL-EMP-NET        TO DTL-NET
             WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-DETAIL
             ADD PAYROLL-EMP-HOURS TO WS-TOTAL-HOURS
             ADD PAYROLL-EMP-GROSS to  WS-TOTAL-GROSS
013400       ADD PAYROLL-EMP-NET   TO  WS-TOTAL-NET
             ADD WS-TOTAL-DEDUCTIONS TO WS-GRAND-TOTAL-DEDUCT

013900       READ PAYROLL-IN
014200     END-PERFORM.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-DASH-LINE.
           MOVE DTL-DATE TO TOTAL-PAYDATE.
           MOVE WS-TOTAL-HOURS TO TOTAL-HOURS-WORKED.
           MOVE WS-TOTAL-OVERTIME TO TOTAL-OVERTIME.
           MOVE WS-TOTAL-GROSS TO TOTAL-GROSS.
           MOVE WS-GRAND-TOTAL-DEDUCT TO TOTAL-DEDUCT.
           MOVE WS-TOTAL-NET TO TOTAL-NET-RPT.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-TOTAL-LINE
015300     CLOSE PAYROLL-IN
015400           PAYROLL-OUT.
015500     DISPLAY "HW16NB CONCLUDES".
015600     GOBACK.

       1000-INITIALIZE.
           OPEN INPUT  PAYROLL-IN.
011100     DISPLAY "WS-PAYROLL-IN-STATUS AFTER OPEN: "
011200              WS-PAYROLL-IN-STATUS.
011300     OPEN OUTPUT PAYROLL-OUT.
           OPEN OUTPUT PAYROLL-RPT.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-HDR1.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-HDR2.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-HDR3.
           MOVE FUNCTION CURRENT-DATE (5:6) TO WS-DATE-MM.
           MOVE FUNCTION CURRENT-DATE (7:8) TO WS-DATE-DD.
           MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YY.
           DISPLAY FUNCTION current-date.
           MOVE WS-DATE TO WS-PAYROLL-RPT-DATE.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-HDR4.
           WRITE PAYROLL-REC-OUT-RPT FROM WS-PAYROLL-RPT-HDR5.

011400     MOVE ZEROES TO WS-TOTAL-FIELDS.
           MOVE ZEROES TO DTL-OVERTIME.
           MOVE ZEROES TO WS-TOTAL-OVERTIME.


