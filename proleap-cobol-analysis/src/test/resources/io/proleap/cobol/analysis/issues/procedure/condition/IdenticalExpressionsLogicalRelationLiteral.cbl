 IDENTIFICATION DIVISION.
 PROGRAM-ID. COND.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 SOMECOND1 PICTURE IS 9(1).
 01 SOMECOND2 PICTURE IS 9(1).
 PROCEDURE DIVISION.
    IF 'Hello' = 'Hello' THEN
       DISPLAY 'thentext'
    ELSE
       DISPLAY 'elsetext'
   	END-IF.
    IF 1 < 2 THEN
       DISPLAY 'thentext'
    ELSE
       DISPLAY 'elsetext'
   	END-IF.
   	IF 2 <> 2 THEN
       DISPLAY 'thentext'
    ELSE
       DISPLAY 'elsetext'
   	END-IF.
       
