       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUSEEMPLOYEES.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ACMEFILE ASSIGN TO "ACME.DAT"
           FILE STATUS IS ACME-FILE-CHECK-KEY
		   ORGANIZATION IS LINE SEQUENTIAL.

	   SELECT FUSEINCFILE ASSIGN TO "FUSESINC.DAT"
           FILE STATUS IS FUSE-FILE-CHECK-KEY
	       ORGANIZATION IS LINE SEQUENTIAL.
    
       SELECT SORTEDFILE ASSIGN TO "SORTED.NEW"
           ORGANIZATION IS LINE SEQUENTIAL.
		
       SELECT WORKFILE ASSIGN TO "WORK.TMP".
	               
       DATA DIVISION.
       FILE SECTION.
	   FD ACMEFILE.
	   01 ACMEDETAILS     PIC X(50).
	
       FD FUSEINCFILE.
	   01 FUSEINCDETAILS  PIC X(50).
				
       FD SORTEDFILE.
	   01 SORTDETAILS        PIC X(50).	
				
       SD WORKFILE.
       01 WORKREC.
          	02 WEMPLOYEE-ID   PIC 9(7).
			02 WEMPLOYEELNAME PIC X(10).
			02 WEMPLOYEEFNAME PIC X(10).
			02 FILLER        PIC X(17).
		    02 WGENRE        PIC X(1).		  
            02 FILLER        PIC X.	   
   
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.   
   
	   01  WS-WORK-AREAS.
	       05  ACME-FILE-CHECK-KEY   PIC X(2).
           05  FUSE-FILE-CHECK-KEY    PIC X(2).
		 

       PROCEDURE DIVISION.
       0050-START.
           PERFORM 0100-READ-EMPLOYEES.
           PERFORM 9000-END-PROGRAM.
       0050-END.

       0100-READ-EMPLOYEES.

		   OPEN INPUT ACMEFILE, FUSEINCFILE.
		   				
		   MERGE WORKFILE ON ASCENDING KEY 
		      WEMPLOYEE-ID
		      USING ACMEFILE,FUSEINCFILE
			  GIVING SORTEDFILE.
		   
	   0100-END.
	   
	   9000-END-PROGRAM.
           CLOSE ACMEFILE, FUSEINCFILE.    	 
                
           STOP RUN.         
          END PROGRAM FUSEEMPLOYEES.