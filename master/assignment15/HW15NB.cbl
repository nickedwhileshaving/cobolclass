000100 identification division.
000200 program-id. HW15NB.
       author. NIKOLINA BEST.
       date-written. 04/20/2018.
000300* Reads file of rug orders; calculate area and perimeter.
000400* Determines price of rug.
000500* Write out new file with results.
000600* The file will be used as input into a report program.
000700
000800 ENVIRONMENT DIVISION.
000900 CONFIGURATION SECTION.
001000 INPUT-OUTPUT SECTION.
001100 FILE-CONTROL.
001200     SELECT SHAPE-FILE-IN
001300     assign to
           "C:\school\cobol\cobolclass\master\data\MATHSHPSBIGGER.dat"
           organization is line sequential
           FILE STATUS IS WS-SHAPE-FILE-IN-STATUS.
001400 
001700     SELECT SHAPE-FILE-OUT
           ASSIGN TO
           "C:\school\cobol\cobolclass\master\data\MATHOUT.dat"
           ORGANIZATION IS LINE SEQUENTIAL
001800     FILE STATUS IS WS-SHAPE-FILE-OUT-STATUS.
001900
002105     SELECT SHAPE-REPORT-OUT
           ASSIGN TO
           "C:\school\cobol\cobolclass\master\data\SHAPERPT.rpt"
           ORGANIZATION IS LINE SEQUENTIAL
001800     FILE STATUS IS WS-SHAPE-REPORT-STATUS.
001900 
002110*  IMPORTANT:  AN ADDITIONAL SELECT STATEMENT IS NEEDED
002120*              HERE FOR THE REPORT.  AN ASSOCIATED FD STATEMENT
002121*              IS ALSO NEEDED BELOW.
002130
002200 DATA DIVISION.
002300 FILE SECTION.
002400 FD  SHAPE-FILE-IN
002500     RECORDING MODE IS F
002600     DATA RECORD IS SHAPE-REC-IN.
002700 01 SHAPE-REC-IN              PIC X(21).
002800
002900 FD  SHAPE-FILE-OUT
003000     RECORDING MODE IS F
003100     DATA RECORD IS SHAPE-REC-OUT.
003200 01 SHAPE-REC-OUT             PIC X(39).

       FD  SHAPE-REPORT-OUT
003000     RECORDING MODE IS F
003100     DATA RECORD IS SHAPE-REC-OUT-RPT.
003200 01 SHAPE-REC-OUT-RPT         PIC X(133).
003320
003400 working-storage section.
003500 01  CALCULATED-SHAPES-RECORD.
003600     05 SHAPE-TYPE             PIC X(10).
003700     05 SIZE-1                 PIC 9(03).
003800     05 SIZE-2                 PIC 9(03).
003900     05 SQ-FT-PRICE            PIC 9(03)V99.
004000     05 CALCULATED-FIELDS-OUT.
004100        10 AREA-OUT            PIC 9(06)V99.
004200        10 PERIMETER-OUT       PIC 9(04).
004300        10 PRICE-OUT           PIC 9(06)V99.
004400
004500 01  WS-CALCULATION-FIELDS.
004600     05 WS-RECORD-COUNT-IN     PIC 9(02).
004700     05 WS-RECORD-COUNT-OUT    PIC 9(02).
004800     05 WS-TOTAL-FILE-COST     PIC 9(06)V99.
004900
005000 01  WS-COST-OUT               PIC $ZZZ,ZZZ.99.
005100
005200 01  WS-SHAPE-FILE-IN-STATUS   pic X(02).
005300     88 SHAPE-FILE-IN-SUCCESSFUL         VALUE "00".
005400     88 END-OF-SHAPE-FILE                VALUE "10".
005500     88 INVALID-SHAPE-IN-FILE            VALUE "11" THRU "99".
005600     88 SHAPE-FILE-NOT-READY             VALUE "01" THRU "99".
005700
005800 01  WS-SHAPE-FILE-OUT-STATUS  pic X(02).
005900     88 GOOD-SHAPE-FILE-WRITE            VALUE "00".

       01  WS-SHAPE-REPORT-STATUS  pic X(02).
005900     88 GOOD-SHAPE-REPORT-WRITE          VALUE "00".
006000
006100 01  WS-RULER                   PIC X(39)
006200     VALUE "----+----1----+----2----+----3----+----".

       01  PAGE-HDG-01.
           05 PAGE-HDG-01-CC          PIC X(01).
           05 FILLER                  PIC X(62) VALUE SPACES.
           05 FILLER                  PIC X(12) VALUE 'SHAPE REPORT'.
           05 FILLER                  PIC X(48) VALUE SPACES.
       01  PAGE-HDG-02.
           05 FILLER                  PIC X(59) VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE 
                                                "RUN DATE: ".
           05 PAGE-HDG-02-YY          PIC X(02).
           05 FILLER                  PIC X(01) VALUE "/".
           05 PAGE-HDG-02-MM          PIC X(02).
           05 FILLER                  PIC X(01) VALUE "/".
           05 PAGE-HDG-02-DD          PIC X(02).
           05 FILLER                  PIC X(43) VALUE SPACES.
       01 PAGE-HDG-03.
           05 PAGE-HDG-03-CC          PIC X(01).
           05 FILLER                  PIC X(21) VALUE SPACES.
           05 FILLER                  PIC X(18) VALUE "SHAPE".
           05 FILLER                  PIC X(01) VALUE SPACES.
           05 FILLER                  PIC X(06) VALUE "SIDE 1".
           05 FILLER                  PIC X(07) VALUE SPACES.
           05 FILLER                  PIC X(06) VALUE "SIDE 2".
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 FILLER                  PIC X(04) VALUE "AREA".
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 FILLER                  PIC X(09) VALUE "PERIMETER".
           05 FILLER                  PIC X(13) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE "PRICE".
           05 FILLER                  PIC X(20) VALUE SPACES.
       01 SHAPE-DETAIL-LINE.
           05 DTL-CC                  PIC X(01).
           05 FILLER                  PIC X(19) VALUE SPACES.
           05 DTL-SHAPE               PIC X(10).
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-SIDE-1        PIC ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-SIDE-2        PIC ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-AREA          PIC ZZZ,ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-PERIMETER     PIC ZZZ,ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-PRICE         PIC Z,ZZZ,ZZZ.99.
           05 FILLER                  PIC X(10) VALUE SPACES.

       01 DASHED-LINE.
           05 FILLER                  PIC X(20) VALUE SPACES.
           05 DSHD-LINE               PIC X(93) VALUE ALL "-".
           05 FILLER                  PIC X(21) VALUE SPACES.

       01 SHAPE-TOTALS-LINE.
           05 TOTAL-LINE-CC           PIC X(01).
           05 FILLER                  PIC X(19) VALUE SPACES.
           05 FILLER                  PIC X(19) VALUE "FILE TOTALS".
           05 FILLER                  PIC X(20) VALUE SPACES.
           05 TOTAL-LINE-COUNT        PIC Z,ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 TOTAL-LINE-AREA         PIC ZZZ,ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 TOTAL-LINE-PERIMETER    PIC ZZZ,ZZ9.
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 TOTAL-LINE-PRICE        PIC $$,ZZZ,ZZ9.99.
           05 FILLER                  PIC X(10) VALUE SPACES.

       01 WS-RUN-DATE                 PIC X(08).
       01 WS-REPORT-RULER.
           05 FILLER                   PIC X(50)
           VALUE "----+----1----+----2----+----3----+----4----+----5".
           05 FILLER                   PIC X(50)
           VALUE "----+----6----+----7----+----8----+----9----+----0".
           05 FILLER                   PIC X(133)
           VALUE "----+----1----+----2----+----3---".

006300
006400 PROCEDURE DIVISION.
006500 0000-DRIVER.
006600
006700     DISPLAY 'START MATHSHPS'.
006800
006900     PERFORM 1000-INITIALIZE.
007000     PERFORM 2000-READ-SHAPES.
007100
007200     PERFORM UNTIL END-OF-SHAPE-FILE
007300       perform 3000-CALCULATE-FIELDS
007400       perform 4000-WRITE-SHAPES
007500       perform 2000-READ-SHAPES
007600     END-PERFORM.
007700     WRITE SHAPE-REC-OUT FROM WS-RULER.
007900
008000     DISPLAY " FILE COST      : " WS-COST-OUT.
008100     DISPLAY " RECORDS READ   : " WS-RECORD-COUNT-IN.
008200     DISPLAY " RECORDS WRITTEN: " WS-RECORD-COUNT-OUT.
008300     DISPLAY "END OF MATHSHPS".
008400
           PERFORM 6000-FINISH-UP.
008700
008800     goback.

       1000-INITIALIZE.
011300     OPEN INPUT  SHAPE-FILE-IN.
011400     OPEN OUTPUT SHAPE-FILE-OUT.
           OPEN OUTPUT SHAPE-REPORT-OUT.
011500     MOVE ZEROES to WS-CALCULATION-FIELDS
011600                    CALCULATED-FIELDS-OUT.
           MOVE FUNCTION CURRENT-DATE (3:4) TO PAGE-HDG-02-YY.
           MOVE FUNCTION CURRENT-DATE (5:6) TO PAGE-HDG-02-MM.
           MOVE FUNCTION CURRENT-DATE (7:8) TO PAGE-HDG-02-DD.
011700     WRITE SHAPE-REC-OUT FROM WS-RULER.
           WRITE SHAPE-REC-OUT-RPT FROM PAGE-HDG-01.
           WRITE SHAPE-REC-OUT-RPT FROM PAGE-HDG-02.
           WRITE SHAPE-REC-OUT-RPT FROM PAGE-HDG-03.

       2000-READ-SHAPES.
009800     read SHAPE-FILE-IN into CALCULATED-SHAPES-RECORD
009900       at end
010000         display "END OF SHAPE FILE"
010100       not AT end
010200         add 1 to WS-RECORD-COUNT-IN.
008900
009000 3000-CALCULATE-FIELDS.
009100     COMPUTE AREA-OUT      = SIZE-1 * SIZE-2.
009200     COMPUTE PERIMETER-OUT = (SIZE-1 * 2) + (SIZE-2 * 2).
009300     COMPUTE PRICE-OUT     = AREA-OUT * SQ-FT-PRICE.
009400     compute WS-TOTAL-FILE-COST
009500                           = WS-TOTAL-FILE-COST + PRICE-OUT.
010300
010400 4000-WRITE-SHAPES.
010500     WRITE SHAPE-REC-OUT FROM CALCULATED-SHAPES-RECORD.
010600     if GOOD-SHAPE-FILE-WRITE
010700        add 1 to WS-RECORD-COUNT-OUT
010800     else
010900        display "BAD WRITE - FILE STATUS: "
011000          WS-SHAPE-FILE-OUT-STATUS.
           MOVE SHAPE-TYPE TO DTL-SHAPE.
           MOVE SIZE-1 TO DTL-SHAPE-SIDE-1.
           MOVE SIZE-2 TO DTL-SHAPE-SIDE-2.
           MOVE PERIMETER-OUT TO DTL-SHAPE-PERIMETER.
           MOVE PRICE-OUT TO DTL-SHAPE-PRICE.
           MOVE AREA-OUT TO DTL-SHAPE-AREA.
           WRITE SHAPE-REC-OUT-RPT FROM SHAPE-DETAIL-LINE.
010600     if GOOD-SHAPE-FILE-WRITE
010700        add 1 to WS-RECORD-COUNT-OUT
010800     else
010900        display "BAD WRITE - FILE STATUS: "
011000          WS-SHAPE-FILE-OUT-STATUS.

       6000-FINISH-UP.
           MOVE WS-TOTAL-FILE-COST to WS-COST-OUT.
           WRITE SHAPE-REC-OUT-RPT FROM DASHED-LINE.
           WRITE SHAPE-REC-OUT-RPT FROM SHAPE-TOTALS-LINE.
           close SHAPE-FILE-IN
008600           SHAPE-FILE-OUT
                 SHAPE-REPORT-OUT.
