 IDENTIFICATION DIVISION.
 PROGRAM-ID. IFSTMT.
 PROCEDURE DIVISION.
    IF '1234' = '1234' THEN DISPLAY 'equal' END-IF.
    IF '1234' <= '1234' THEN DISPLAY 'lesser equal' END-IF.
    IF '1234' >= '1234' THEN DISPLAY 'greater equal' END-IF.

    IF '1234' < '2341' THEN DISPLAY 'lesser' END-IF.
    IF '1234' > '2341' THEN DISPLAY 'greater (should not display)' END-IF.

    IF '2341' > '1234' THEN DISPLAY 'greater' END-IF.
    IF '2341' < '1234' THEN DISPLAY 'lesser (should not display)' END-IF.