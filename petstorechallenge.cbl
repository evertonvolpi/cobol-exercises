       IDENTIFICATION DIVISION.
       PROGRAM-ID. PETSTORECHALLENGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PETSALESFILE ASSIGN TO "PETSTORESALES.DAT"
		   ORGANIZATION IS LINE SEQUENTIAL.
       SELECT PETSALESREPORT ASSIGN TO "PETSALESREPORT.DAT".
               
       DATA DIVISION.
	   FILE SECTION.
       FD PETSALESFILE.
	   01 SALESDETAILS.
		   88 ENDOFSALESFILE VALUE HIGH-VALUES.
           02 CUSTOMER-ID      PIC 9(7).
		   02 CUSTOMERNAME.
		      05  LASTNAME     PIC X(15).
		      05  FIRSTNAME    PIC X(15).
           02 PETITEM OCCURS 3 TIMES.
	   	      05 DESCRIPTION   PIC X(20).
			  05 PRICE         PIC 999999V99.
              05 QUANTITY      PIC 99999.
				   
       FD PETSALESREPORT.
       01 VALID-SALES-RECORD   PIC X(80).

       WORKING-STORAGE SECTION.
       
       01  WS-A PIC 9(1) VALUE 1.

	   01  WS-FIELDS.
		   05  WS-TOTAL-QUANT  PIC 999.
		   05  WS-QUANT-C      PIC 999.
		   05  WS-ITEM-TOTAL   PIC 9999V99.
		   05  WS-TOTAL-SALE   PIC 99999V99.
		   05  WS-SALE-C       PIC 99999V99.
    
       01  WS-DATE.
           05  WS-YEAR         PIC 99.
           05  WS-MONTH        PIC 99.
           05  WS-DAY          PIC 99.	   
		   
       01  HEADING-LINE-1.
           05 FILLER           PIC X(46) VALUE SPACES.
           05 FILLER           PIC X(21) VALUE 'PET SUPPLIES AND MORE'.
       
       01  HEADING-LINE-2.
           05 FILLER           PIC X(16) VALUE 'ITEM DESCRIPTION'.
           05 FILLER           PIC X(20) VALUE SPACES.
           05 FILLER           PIC X(11)  VALUE 'PRICE'.
           05 FILLER           PIC X(2) VALUE SPACES.
           05 FILLER           PIC X(11)  VALUE 'QUANTITY'.
           05 FILLER           PIC X(2) VALUE SPACES.
           05 FILLER           PIC X(11)  VALUE 'TOTAL'.
        		
	   01  DETAIL-LINE.
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 DET-DESCRIPTION  PIC X(20).
           05 FILLER           PIC X(9)  VALUE SPACES.
           05 DET-PRICE        PIC $,$$9.99.
           05 FILLER           PIC X(8)  VALUE SPACES.
           05 DET-QUANTITY     PIC Z9.
           05 FILLER           PIC X(7)  VALUE SPACES.
           05 DET-ITEM-TOTAL   PIC $$,$$9.99.

       01  DETAIL-CUSTOMER-LINE-1.
           05 FILLER           PIC X(20) VALUE SPACES.
           05 FILLER           PIC X(20) VALUE '===================='.
           05 FILLER           PIC X(20) VALUE '===================='.
           05 FILLER           PIC X(20) VALUE '===================='.
		           
       01  DETAIL-CUSTOMER-LINE-2.
           05 FILLER           PIC X(20) VALUE SPACES.
           05 DET-CUSTOMER     PIC X(15).
           05 FILLER           PIC X(10) VALUE 'QUANTITY: '.
           05 DET-QUANTITY-C   PIC 99999.
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 FILLER           PIC X(10) VALUE 'SUB-TOTAL:'.
           05 DET-PRICE-C      PIC $$$,$$9.99.
           05 FILLER           PIC X(2)  VALUE SPACES.
       
       01  BLANK-LINE.
           05 FILLER           PIC X(80) VALUE SPACES.

	   01  DETAIL-TOTAL-LINE.
           05 FILLER           PIC X(7) VALUE SPACES.
	       05 FILLER           PIC X(19)  VALUE 
	          "    TOTAL QUANITY: ".
	       05 DET-TOTAL-QUANT  PIC 999. 
	       05 FILLER           PIC XX.
	       05 FILLER           PIC X(23)  VALUE 
	          "TOTAL AMOUNT: ".
	       05 FILLER           PIC X(1)  VALUE SPACES.
	       05 DET-TOT-SALES    PIC $$,$$$,$$9.99.
	       05 FILLER           PIC X(3)  VALUE SPACES.
		
       PROCEDURE DIVISION.
       0100-START.
           OPEN INPUT PETSALESFILE. 
           OPEN OUTPUT PETSALESREPORT. 
            READ PETSALESFILE
			  AT END SET ENDOFSALESFILE TO TRUE
			  END-READ.
      *     DISPLAY HEADING-LINE-1.
           WRITE VALID-SALES-RECORD FROM HEADING-LINE-1
               AFTER ADVANCING 1 LINE.
           WRITE VALID-SALES-RECORD FROM HEADING-LINE-2
               AFTER ADVANCING 1 LINE.
      *     DISPLAY HEADING-LINE-2.
		   
		   PERFORM 0200-PROCESS-CUSTOMER UNTIL ENDOFSALESFILE
		   PERFORM 0290-PRINT-TOTAL.
		   PERFORM 0300-STOP-RUN.
	   0100-END.	
		   
       0200-PROCESS-CUSTOMER.

           MOVE 0 TO WS-SALE-C.
           MOVE 0 TO WS-QUANT-C.
           MOVE 1 TO WS-A.

           PERFORM 0220-PROCESS-ITEMS 3 TIMES.
			
		   COMPUTE WS-TOTAL-SALE = WS-TOTAL-SALE + WS-SALE-C.
		   COMPUTE WS-TOTAL-QUANT = WS-TOTAL-QUANT + WS-QUANT-C.
           
           MOVE LASTNAME TO DET-CUSTOMER.
           MOVE WS-QUANT-C TO DET-QUANTITY-C.
           MOVE WS-SALE-C TO DET-PRICE-C.

           WRITE VALID-SALES-RECORD FROM DETAIL-CUSTOMER-LINE-1
               AFTER ADVANCING 1 LINE.               
           WRITE VALID-SALES-RECORD FROM DETAIL-CUSTOMER-LINE-2
               AFTER ADVANCING 1 LINE.
           WRITE VALID-SALES-RECORD FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
      *     DISPLAY DETAIL-CUSTOMER-LINE-1.
      *     DISPLAY DETAIL-CUSTOMER-LINE-2.

           READ PETSALESFILE
			  AT END SET ENDOFSALESFILE TO TRUE
			  END-READ.

       0200-END.

       0220-PROCESS-ITEMS.

           MOVE DESCRIPTION(WS-A) TO DET-DESCRIPTION.
		   MOVE PRICE(WS-A) TO DET-PRICE.
		   MOVE QUANTITY(WS-A) TO DET-QUANTITY.

	   	   COMPUTE WS-ITEM-TOTAL = PRICE(WS-A) * QUANTITY(WS-A).
           COMPUTE WS-QUANT-C = WS-QUANT-C + QUANTITY(WS-A).
           COMPUTE WS-SALE-C = WS-SALE-C + WS-ITEM-TOTAL.
		   
		   MOVE WS-ITEM-TOTAL TO DET-ITEM-TOTAL.
           WRITE VALID-SALES-RECORD FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE.
      *     DISPLAY DETAIL-LINE.  

           ADD 1 TO WS-A.

       0220-END.
	   
       0290-PRINT-TOTAL. 			
		     
		   MOVE WS-TOTAL-QUANT TO DET-TOTAL-QUANT.
		   MOVE WS-TOTAL-SALE TO DET-TOT-SALES.
		 
           WRITE VALID-SALES-RECORD FROM DETAIL-TOTAL-LINE.
      *     DISPLAY DETAIL-TOTAL-LINE.
		   		   
	   0290-END.
		
       0300-STOP-RUN.
	       CLOSE PETSALESFILE.
           CLOSE PETSALESREPORT.
           STOP RUN.
           
       END PROGRAM PETSTORECHALLENGE.
