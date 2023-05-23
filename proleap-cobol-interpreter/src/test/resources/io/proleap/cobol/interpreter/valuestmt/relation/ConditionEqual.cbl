 IDENTIFICATION DIVISION.
 PROGRAM-ID. IFSTMT.
 PROCEDURE DIVISION.
    IF '1234' = '1234' THEN DISPLAY 'same string' END-IF.
    IF '1234' = '4321' THEN DISPLAY 'different string (should not display)' END-IF.
    IF '1234' = 1234 THEN DISPLAY 'string vs decimal' END-IF.

    IF 1234 = 1234 THEN DISPLAY 'same decimal' END-IF.
    IF 1234 = 4321 THEN DISPLAY 'different decimal (should not display)' END-IF.

    IF True = True THEN DISPLAY 'same true boolean' END-IF.
    IF False = False THEN DISPLAY 'same false boolean' END-IF.
    IF True = False THEN DISPLAY 'different boolean (should not display)' END-IF.

    IF HIGH-VALUE = HIGH-VALUE THEN DISPLAY 'same high-value' END-IF.
    IF HIGH-VALUE = LOW-VALUE THEN DISPLAY 'high-value low-value (should not display)' END-IF.
    IF LOW-VALUE = HIGH-VALUE THEN DISPLAY 'low-value high-value (should not display)' END-IF.
    IF LOW-VALUE = LOW-VALUE THEN DISPLAY 'same low-value' END-IF.
