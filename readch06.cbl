       IDENTIFICATION DIVISION.
       PROGRAM-ID. READCH06.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT BASEBALL ASSIGN TO "BASEBALLIDX.DAT"
           FILE STATUS IS FILE-CHECK-KEY
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS GAMEID
           ALTERNATE RECORD KEY IS GAMEDATE
               WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.
       FD BASEBALL.
       01 GAMERECORD.
           88 ENDOFFILE        VALUE HIGH-VALUES.
           05 GAMEID           PIC X(36).
           05 GAMEYEAR         PIC 9(4).
           05 GAMEDATE         PIC X(10).
           05 GAMETIME         PIC X(13).
           05 GAMEATTENDANCE   PIC X(5).
           05 GAMEHOMETEAM     PIC X(12).
           05 GAMEAWAYTEAM     PIC X(12).
           05 GAMEMOREINFO     PIC X(35).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05 FILE-CHECK-KEY   PIC X(2).
               88 RECORDFOUND  VALUE "00".

           05 READTYPE         PIC 9.
               88 GAMEIDKEY       VALUE 1.
               88 GAMEDATEKEY     VALUE 2.
               88 ALLGAMESKEY     VALUE 3.

           05 PRINTGAME.
               10 PRTGAMEID            PIC X(36).
               10 PRTGAMEYEAR          PIC 9(4).
               10 PRTGAMEDATE          PIC X(10).
               10 PRTGAMETIME          PIC X(13).
               10 PRTGAMEATTENDANCE    PIC X(5).
               10 PRTGAMEHOMETEAM      PIC X(12).
               10 PRTGAMEAWAYTEAM      PIC X(12).
               10 PRTGAMEMOREINFO      PIC X(35).

       PROCEDURE DIVISION.
       0100-START.
           OPEN INPUT BASEBALL.
           DISPLAY "ID (1): ".
           DISPLAY "DATE (2): ".

           ACCEPT READTYPE.

           IF GAMEIDKEY
               DISPLAY "ID (36 DIGITS): "
                   WITH NO ADVANCING
               ACCEPT GAMEID
               READ BASEBALL
                   KEY IS GAMEID
                   INVALID KEY DISPLAY "ERROR: ", FILE-CHECK-KEY
               END-READ
           END-IF

           IF GAMEDATEKEY
               DISPLAY "DATE (10 DIGITS YYYY-MM-DD): "
                   WITH NO ADVANCING
               ACCEPT GAMEDATE
               READ BASEBALL
                   KEY IS GAMEDATE
                   INVALID KEY DISPLAY "ERROR: ", FILE-CHECK-KEY
               END-READ
           END-IF.

           IF RECORDFOUND
               MOVE GAMEID TO PRTGAMEID
               MOVE GAMEYEAR       TO PRTGAMEYEAR      
               MOVE GAMEDATE       TO PRTGAMEDATE       
               MOVE GAMETIME       TO PRTGAMETIME       
               MOVE GAMEATTENDANCE TO PRTGAMEATTENDANCE
               MOVE GAMEHOMETEAM   TO PRTGAMEHOMETEAM   
               MOVE GAMEAWAYTEAM   TO PRTGAMEAWAYTEAM   
               MOVE GAMEMOREINFO   TO PRTGAMEMOREINFO

               DISPLAY PRINTGAME
           END-IF.

           PERFORM 9000-END-PROGRAM.

       0100-END.      

       9000-END-PROGRAM.
           CLOSE BASEBALL.
           STOP RUN.
           END PROGRAM READCH06.
