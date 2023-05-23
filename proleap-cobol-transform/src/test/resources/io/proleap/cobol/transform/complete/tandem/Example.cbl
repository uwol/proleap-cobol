 IDENTIFICATION DIVISION.
 PROGRAM-ID. EXAMPLE.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 SOME-ITEM.
    05 ITEM-NAME PIC X(20) VALUE "Item Name".
    05 PRICE PIC 999V99 VALUE 99.99.
    05 AMOUNT PIC 999 VALUE 42.
 01 SOME-PERSON.
    05 PERSON-NAME PIC X(20) VALUE "Grace Hopper".
    05 PERSON-ADDRESS.
       10 STREET PIC X(20).
       10 CITY VALUE "Arlington".
 77 TOTAL-AMOUNT          PIC 99999V99.
 77 DISCOUNT-BOUNDARY     PIC 99999V99 VALUE 1000.00.
 77 DISCOUNT-PERCENT      PIC 99 VALUE 10.
 77 DISCOUNT-AMOUNT       PIC 99999V99.

 PROCEDURE DIVISION.
 BATCH-DISCOUNT.
   PERFORM COMPUTE-DISCOUNT.
   PERFORM DISPLAY-DISCOUNT.
   STOP RUN.
 COMPUTE-DISCOUNT.
   MULTIPLY AMOUNT BY PRICE GIVING TOTAL-AMOUNT.
   IF TOTAL-AMOUNT > DISCOUNT-BOUNDARY
     MULTIPLY TOTAL-AMOUNT BY DISCOUNT-PERCENT GIVING DISCOUNT-AMOUNT
     DIVIDE 100 INTO DISCOUNT-AMOUNT
     SUBTRACT DISCOUNT-AMOUNT FROM TOTAL-AMOUNT.
 DISPLAY-DISCOUNT.
   DISPLAY PERSON-NAME.
   DISPLAY "Total: ", TOTAL-AMOUNT.
   DISPLAY "Discount: ", DISCOUNT-AMOUNT.