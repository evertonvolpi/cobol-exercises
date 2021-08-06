       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "OBTAININTPUT".
       AUTHOR.     EVERTON VOLPI.
      *This is a comment 
      *Columns 8-11 A Margin
      *Columns 12-72 B Margin 
       ENVIRONMENT DIVISION.
            
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAME PIC A(20).

       PROCEDURE DIVISION.
           0100-START-HERE.
               DISPLAY "Please enter your name".
               ACCEPT NAME.
               DISPLAY "It is nice to meet you, ", NAME.               
       STOP RUN.
       END PROGRAM OBTAININTPUT.
