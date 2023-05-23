 IDENTIFICATION DIVISION.
 PROGRAM-ID. IFSTMT.
 PROCEDURE DIVISION.
    IF '1234' <> '4321' THEN DISPLAY 'different string' END-IF.
    IF '1234' NOT EQUAL '4321' THEN DISPLAY 'different string' END-IF.

    IF '1234' <> '1234' THEN DISPLAY 'same string (should not display)' END-IF.
    IF '1234' NOT EQUAL '1234' THEN DISPLAY 'same string (should not display)' END-IF.

    IF '1234' <> 1234 THEN DISPLAY 'string vs decimal (should not display)' END-IF.
    IF '1234' NOT EQUAL 1234 THEN DISPLAY 'string vs decimal (should not display)' END-IF.

    IF 1234 <> 4321 THEN DISPLAY 'different decimal' END-IF.
    IF 1234 NOT EQUAL 4321 THEN DISPLAY 'different decimal' END-IF.

    IF 1234 <> 1234 THEN DISPLAY 'same decimal (should not display)' END-IF.
    IF 1234 NOT EQUAL 1234 THEN DISPLAY 'same decimal (should not display)' END-IF.

    IF True <> False THEN DISPLAY 'different boolean' END-IF.
    IF True NOT EQUAL False THEN DISPLAY 'different boolean' END-IF.

    IF True <> True THEN DISPLAY 'same true boolean (should not display)' END-IF.
    IF True NOT EQUAL True THEN DISPLAY 'same true boolean (should not display)' END-IF.

    IF False <> False THEN DISPLAY 'same false boolean (should not display)' END-IF.
    IF False NOT EQUAL False THEN DISPLAY 'same false boolean (should not display)' END-IF.

    IF HIGH-VALUE <> LOW-VALUE THEN DISPLAY 'high-value low-value' END-IF.
    IF HIGH-VALUE NOT EQUAL LOW-VALUE THEN DISPLAY 'high-value low-value' END-IF.

    IF HIGH-VALUE <> HIGH-VALUE THEN DISPLAY 'high-value (should not display)' END-IF.
    IF HIGH-VALUE NOT EQUAL HIGH-VALUE THEN DISPLAY 'high-value (should not display)' END-IF.

    IF HIGH-VALUE <> LOW-VALUE THEN DISPLAY 'low-value high-value' END-IF.
    IF HIGH-VALUE NOT EQUAL LOW-VALUE THEN DISPLAY 'low-value high-value' END-IF.

    IF LOW-VALUE <> LOW-VALUE THEN DISPLAY 'low-value (should not display)' END-IF.
    IF LOW-VALUE NOT EQUAL LOW-VALUE THEN DISPLAY 'low-value (should not display)' END-IF.
    