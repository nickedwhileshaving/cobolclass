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
001300     assign to "C:\school\cobol\data\MATHSHPSBIGGER.dat"
           organization is line sequential
           FILE STATUS IS WS-SHAPE-FILE-IN-STATUS.
001400 
001700     SELECT SHAPE-FILE-OUT
           ASSIGN TO "C:\school\cobol\data\MATHOUT.dat"
           ORGANIZATION IS LINE SEQUENTIAL
001800     FILE STATUS IS WS-SHAPE-FILE-OUT-STATUS.
001900
002105     SELECT SHAPE-REPORT-IN
           ASSIGN TO "C:\school\cobol\data\SHAPERPT.rpt"
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

       FD  SHAPE-REPORT-IN
003000     RECORDING MODE IS F
003100     DATA RECORD IS SHAPE-REC-OUT.
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
006300
006400 PROCEDURE DIVISION.
006500 0000-DRIVER.
006600
006700     DISPLAY 'START MATHSHPS'.
006800
006900     PERFORM XXXX-INITIALIZE.
007000     PERFORM XXXX-READ-SHAPES.
007100
007200     perform until END-OF-SHAPE-FILE
007300       perform XXXX-CALCULATE-FIELDS
007400       perform XXXX-WRITE-SHAPES
007500       perform XXXX-READ-SHAPES
007600     END-PERFORM.
007700     write SHAPE-REC-OUT from WS-RULER.
007800     move WS-TOTAL-FILE-COST to WS-COST-OUT.
007900
008000     display " FILE COST      : " WS-COST-OUT.
008100     display " RECORDS READ   : " WS-RECORD-COUNT-IN.
008200     display " RECORDS WRITTEN: " WS-RECORD-COUNT-OUT.
008300     display "END OF MATHSHPS".
008400
008500     close SHAPE-FILE-IN
008600           SHAPE-FILE-OUT.
008700
008800     goback.
008900
009000 XXXX-CALCULATE-FIELDS.
009100     COMPUTE AREA-OUT      = SIZE-1 * SIZE-2.
009200     COMPUTE PERIMETER-OUT = (SIZE-1 * 2) + (SIZE-2 * 2).
009300     COMPUTE PRICE-OUT     = AREA-OUT * SQ-FT-PRICE.
009400     compute WS-TOTAL-FILE-COST
009500                           = WS-TOTAL-FILE-COST + PRICE-OUT.
009600
009700 XXXX-READ-SHAPES.
009800     read SHAPE-FILE-IN into CALCULATED-SHAPES-RECORD
009900       at end
010000         display "END OF SHAPE FILE"
010100       not AT end
010200         add 1 to WS-RECORD-COUNT-IN.
010300
010400 XXXX-WRITE-SHAPES.
010500     WRITE SHAPE-REC-OUT FROM CALCULATED-SHAPES-RECORD.
010600     if GOOD-SHAPE-FILE-WRITE
010700        add 1 to WS-RECORD-COUNT-OUT
010800     else
010900        display "BAD WRITE - FILE STATUS: "
011000          WS-SHAPE-FILE-OUT-STATUS.
011100
011200 XXXX-INITIALIZE.
011300     OPEN INPUT  SHAPE-FILE-IN.
011400     OPEN OUTPUT SHAPE-FILE-OUT.
011500     MOVE ZEROES to WS-CALCULATION-FIELDS
011600                    CALCULATED-FIELDS-OUT.
011700     WRITE SHAPE-REC-OUT FROM WS-RULER.
