 IDENTIFICATION DIVISION.
 PROGRAM-ID. COLLABSABLEIFSTMT.
 PROCEDURE DIVISION.
    IF SOMECOND1 THEN
        IF SOMECOND2 THEN
 			IF SOMECOND3 THEN
       				DISPLAY 'thentext'
    			ELSE
       				DISPLAY 'elsetext'
    		END-IF
    	ELSE
       		DISPLAY 'elsetext'
    	END-IF
    ELSE
       DISPLAY 'elsetext'
    END-IF.
    
    
    IF SOMECOND1 THEN
        IF SOMECOND2 THEN
 	   		DISPLAY 'thentext'
 		ELSE
       		DISPLAY 'elsetext'
    	END-IF
    ELSE
       DISPLAY 'elsetext'
    END-IF.
    
     IF SOMECOND1 THEN
 	   		DISPLAY 'thentext'
 		ELSE
       		DISPLAY 'elsetext'
    END-IF.