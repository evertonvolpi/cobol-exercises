       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH7.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT WEATHER ASSIGN TO "weather2020.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD WEATHER.
       01 WEATHERDETAILS.
           88 ENDOFFILE VALUE HIGH-VALUES.
           02 LINEID           PIC 9(6).
           02 LINEMONTH        PIC 9(2).
           02 LINEDAY          PIC 9(2).
           02 LINEYEAR         PIC 9(4).
           02 LINEMM           PIC 9(2).
           02 LINERAINED.
               03 RAINED       PIC X(5)
                   OCCURS 6 TIMES.

       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 TOTALLINES       PIC 999 VALUE ZEROES.
           05 RAININFO OCCURS 100 TIMES.
      *     KEY IS TABLEID.
      *     INDEXED BY WEATHERINDEX.
               10 TABLEID      PIC 9(6).
               10 TABLEMONTH   PIC 9(2).
               10 TABLEDAY     PIC 9(2).
               10 TABLEYEAR    PIC 9(4).
               10 TABLEMM      PIC 9(2).
               10 TABLEBIRAIN.
                   15 BIMONTH  PIC X(5)
                       OCCURS 6 TIMES.

       01  WS-DETAIL-LINE.
           05 WS-ID      PIC 9(6).
           05 FILLER     PIC XX VALUE SPACES.
           05 WS-MONTH   PIC 9(2).
           05 FILLER     PIC X VALUE "-".
           05 WS-DAY     PIC 9(2).
           05 FILLER     PIC X VALUE "-".
           05 WS-YEAR    PIC 9(4).
           05 FILLER     PIC XX VALUE SPACES.
           05 WS-MM      PIC 9(2).
           05 WS-BIRAIN  OCCURS 6 TIMES.
              07 FILLER     PIC XX VALUE SPACES.
              07 WS-BIMONTH  PIC X(5).

       PROCEDURE DIVISION.
       0100-BEGIN.

           OPEN INPUT WEATHER.
           READ WEATHER
               AT END SET ENDOFFILE TO TRUE
               END-READ.

           COMPUTE TOTALLINES = 1.
           PERFORM 0200-PROCESS-RECORDS UNTIL ENDOFFILE.
           PERFORM 0250-DISPLAY-DETAILS.
           PERFORM 0300-STOP-RUN.


       0100-END.

       0200-PROCESS-RECORDS.
           
           MOVE LINEID TO TABLEID(TOTALLINES).           
           MOVE LINEMONTH TO TABLEMONTH(TOTALLINES).           
           MOVE LINEDAY TO TABLEDAY(TOTALLINES).           
           MOVE LINEYEAR TO TABLEYEAR(TOTALLINES).           
           MOVE LINEMM TO TABLEMM(TOTALLINES).           
           MOVE RAINED(1) TO BIMONTH(TOTALLINES,1).           
           MOVE RAINED(2) TO BIMONTH(TOTALLINES,2).           
           MOVE RAINED(3) TO BIMONTH(TOTALLINES,3).           
           MOVE RAINED(4) TO BIMONTH(TOTALLINES,4).          
           MOVE RAINED(5) TO BIMONTH(TOTALLINES,5).         
           MOVE RAINED(6) TO BIMONTH(TOTALLINES,6).
           COMPUTE TOTALLINES = TOTALLINES + 1.
           READ WEATHER AT END SET ENDOFFILE TO TRUE END-READ.        

       0200-END.
               
       0250-DISPLAY-DETAILS.

           PERFORM VARYING TOTALLINES FROM 1 BY 1
               UNTIL TOTALLINES > 100
               MOVE TABLEID(TOTALLINES) TO WS-ID
               MOVE TABLEMONTH (TOTALLINES)   TO WS-MONTH
               MOVE TABLEDAY (TOTALLINES)     TO WS-DAY
               MOVE TABLEYEAR (TOTALLINES)    TO WS-YEAR
               MOVE TABLEMM (TOTALLINES)      TO WS-MM
               MOVE BIMONTH(TOTALLINES,1) TO WS-BIMONTH(1)           
               MOVE BIMONTH(TOTALLINES,2) TO WS-BIMONTH(2)           
               MOVE BIMONTH(TOTALLINES,3) TO WS-BIMONTH(3)           
               MOVE BIMONTH(TOTALLINES,4) TO WS-BIMONTH(4)          
               MOVE BIMONTH(TOTALLINES,5) TO WS-BIMONTH(5)         
               MOVE BIMONTH(TOTALLINES,6) TO WS-BIMONTH(6)
               DISPLAY WS-DETAIL-LINE
           END-PERFORM.
       0250-END.

       0300-STOP-RUN.
           CLOSE WEATHER.
           STOP RUN.
           END PROGRAM CH7.
