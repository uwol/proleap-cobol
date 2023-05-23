 IDENTIFICATION DIVISION.
 PROGRAM-ID. EXITSTMT.
 PROCEDURE DIVISION.
 INIT.
     PERFORM PROC2 THROUGH PROC4.
     STOP RUN.
 PROC1.
     Display "Proc1".
 PROC2.
     Display "Proc2".
 PROC3.
     Display "Proc3".
     EXIT.
 PROC4.
     Display "Proc4".