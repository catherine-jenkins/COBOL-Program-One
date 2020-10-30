       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProgramOne.
       AUTHOR. R C JENKINS. 
      ****************************************************************
      * This is a program that produces a detailed report of the
      * inventory for Luna, Ltd., a small company that supplies go-cart
      * businesses with parts.
      ****************************************************************
      * INPUT:
      *    The INVENTORY FILE contains the following
      *    data in each record:
      *         1. CATALOG NUMBER
      *         2. ITEM NAME
      *         3. UNIT PURCHASE PRICE
      *         4. QUANTITY ON HAND
      *         5. QUANTITY ON ORDER
      *         6. REORDER POINT
      *         7. WAREHOUSE ID
      *         8. WAREHOUSE BIN
      ****************************************************************
      * OUTPUT:
      *    The INVENTORY REPORT contains the following information:
      *        
      *       DETAIL LINE:
      *         1. CATALOG NUMBER
      *         2. ITEM NAME
      *         3. UNIT PURCHASE PRICE
      *         4. QUANTITY ON HAND
      *         5. QUANTITY ON ORDER
      *         6. REORDER POINT 
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE
               ASSIGN TO 'InventoryFile.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INVENTORY-REPORT-FILE
               ASSIGN TO PRINTER 'InventoryReportFile.txt'.

       DATA DIVISION.
       FILE SECTION.

       FD  INVENTORY-FILE
           RECORD CONTAINS 60 CHARACTERS.

       01  PART-RECORD.
           05  PA_CATALOG_NUM              PIC X(5).
           05  PA_PART_NAME                PIC X(15).
           05  FILLER                      PIC X(5).
           05  PA_PURCHASE_PRICE           PIC 999V99.
           05  FILLER                      PIC X(6).
           05  PA_QUANTITY_ON_HAND         PIC 9999.
           05  PA_QUANTITY_ON_ORDER        PIC 9999.
           05  PA_REORDER_POINT            PIC 9999.
           05  PA_WAREHOUSE_ID             PIC X(6).
           05  FILLER                      PIC X(1).
           05  PA_WAREHOUSE_BIN            PIC X(5).

       FD  INVENTORY-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS. 

       01  REPORT-RECORD                   PIC X(80).                 

       WORKING-STORAGE SECTION.

       01  WS-E0F-FLAGS.
           05  NO-MORE-DATA                PIC X       VALUE "N".

       01  WS-DATE.
           05 WS-YEAR                      PIC 9999.
           05 WS-MONTH                     PIC 99.
           05 WS-DAY                       PIC 99. 

       01  REPORT-FIELDS.
           05 PROPER-SPACING               PIC 9       VALUE 1.
       
      ********************    OUTPUT AREA    *************************

       01  HEADING-ONE.
           05  H1-DATE.
               10  H1-MONTH            PIC 99.
               10                      PIC X       VALUE "/".
               10  H1-DAY              PIC 99.
               10                      PIC X       VALUE "/".
               10  H1-YEAR             PIC 9999.  
           05                          PIC X(27)   VALUE SPACES.
           05                          PIC X(9)    VALUE "LUNA, LTD".
           05                          PIC X(24)   VALUE SPACES.
           05                          PIC X(3)    VALUE "RCJ".
           05                          PIC X(7)    VALUE SPACES.

       01  HEADING-TWO.
           05                          PIC X(33)   VALUE SPACES.
           05                          PIC X(16)   VALUE 
                                                   "INVENTORY REPORT".
           05                          PIC X(31)   VALUE SPACES.
       
       01  HEADING-THREE.
           05                          PIC X(3)    VALUE SPACES.
           05                          PIC X(7)    VALUE "CATALOG".
           05                          PIC X(8)    VALUE SPACES.
           05                          PIC X(4)    VALUE "PART".
           05                          PIC X(9)    VALUE SPACES.
           05                          PIC x(8)    VALUE "PURCHASE".
           05                          PIC X(3)    VALUE SPACES.
           05                          PIC x(8)    VALUE "QUANTITY".
           05                          PIC X(3)    VALUE SPACES.
           05                          PIC X(8)    VALUE "QUANTITY".
           05                          PIC X(3)    VALUE SPACES.
           05                          PIC X(7)    VALUE "REORDER".
           05                          PIC X(9)    VALUE SPACES.

       01  HEADING-FOUR.
           05                          PIC X(3)    VALUE SPACES.
           05                          PIC X(6)    VALUE "NUMBER".
           05                          PIC X(9)    VALUE SPACES.
           05                          PIC X(4)    VALUE "NAME".
           05                          PIC X(10)   VALUE SPACES.
           05                          PIC X(5)    VALUE "PRICE".
           05                          PIC X(5)    VALUE SPACES.
           05                          PIC X(2)    VALUE "ON".
           05                          PIC X       VALUE SPACES.
           05                          PIC X(4)    VALUE "HAND".
           05                          PIC X(4)    VALUE SPACES.
           05                          PIC X(2)    VALUE "ON".
           05                          PIC X(1)    VALUE SPACES.
           05                          PIC X(5)    VALUE "ORDER".
           05                          PIC X(4)    VALUE SPACES.
           05                          PIC X(5)    VALUE "POINT".
           05                          PIC X(10)   VALUE SPACES.

       01  DETAIL-LINE.
           05                          PIC X(4)    VALUE SPACES.
           05  DL-CATALOG-NUMBER       PIC X(5).
           05                          PIC X(4)    VALUE SPACES.
           05  DL-PART-NAME            PIC X(15).
           05                          PIC X(5)    VALUE SPACES.
           05  DL-PURCHASE-PRICE       PIC 999.99.
           05                          PIC X(5)    VALUE SPACES.
           05  DL-QUANTITY-ON-HAND     PIC 9999.
           05                          PIC X(7)    VALUE SPACES.
           05  DL-QUANTITY-ON-ORDER    PIC 9999.
           05                          PIC X(7)    VALUE SPACES.
           05  DL-REORDER-POINT        PIC 9999.
           05                          PIC X(10)   VALUE SPACES.


       PROCEDURE DIVISION.
       
       10-MAIN-MODULE.
           
           PERFORM 15-HOUSEKEEPING
           PERFORM 20-WRITE-ROUTINE
           PERFORM 25-READ-RECORDS
           PERFORM 40-CLOSE-ROUTINE
        .

       15-HOUSEKEEPING.
           
           OPEN INPUT  INVENTORY-FILE
                OUTPUT INVENTORY-REPORT-FILE
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
        .

       20-WRITE-ROUTINE.

           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE.
           MOVE 2 TO PROPER-SPACING
           MOVE HEADING-TWO TO REPORT-RECORD
           PERFORM 30-WRITE-LINES-ROUTINE
           MOVE 3 TO PROPER-SPACING 
           MOVE HEADING-THREE TO REPORT-RECORD
           PERFORM 30-WRITE-LINES-ROUTINE
           MOVE 1 TO PROPER-SPACING
           MOVE HEADING-FOUR TO REPORT-RECORD
           PERFORM 30-WRITE-LINES-ROUTINE
           MOVE 1 TO PROPER-SPACING
           MOVE SPACES TO REPORT-RECORD
           PERFORM 30-WRITE-LINES-ROUTINE
        .
 
       25-READ-RECORDS.
          
           PERFORM UNTIL NO-MORE-DATA = "Y"
               READ INVENTORY-FILE
                   AT END  
                       MOVE "Y" TO NO-MORE-DATA
                   NOT AT END  
                       PERFORM 35-PROCESS-ROUTINE
               END-READ
           END-PERFORM
        .

       30-WRITE-LINES-ROUTINE.
           
           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
        .

       35-PROCESS-ROUTINE.  

           MOVE PA_CATALOG_NUM TO DL-CATALOG-NUMBER
           MOVE PA_PART_NAME TO DL-PART-NAME
           MOVE PA_PURCHASE_PRICE TO DL-PURCHASE-PRICE
           MOVE PA_QUANTITY_ON_HAND TO DL-QUANTITY-ON-HAND
           MOVE PA_QUANTITY_ON_ORDER TO DL-QUANTITY-ON-ORDER
           MOVE PA_REORDER_POINT TO DL-REORDER-POINT 
           MOVE DETAIL-LINE TO REPORT-RECORD 

           PERFORM 30-WRITE-LINES-ROUTINE       
        .

       40-CLOSE-ROUTINE.
           CLOSE INVENTORY-FILE
           CLOSE INVENTORY-REPORT-FILE

           STOP RUN
        .
