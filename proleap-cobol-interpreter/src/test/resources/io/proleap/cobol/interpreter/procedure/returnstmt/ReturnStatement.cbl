 IDENTIFICATION DIVISION.
 PROGRAM-ID. MakeSummaryFile.

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    SELECT WorkFile ASSIGN TO "WORK.TMP".

    SELECT SalesFile ASSIGN TO "SALES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

    SELECT SalesSummaryFile ASSIGN TO "SUMMARY.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

 DATA DIVISION.
 FILE SECTION.

 FD SalesFile.
 01 SalesRec PIC X(10).

 SD WorkFile.
 01 WorkRec.
    88 EndOfWorkFile VALUE HIGH-VALUES.
    02 SalespsnNumWF PIC X(5).
    02 QtySoldWF PIC 9(4).

 FD SalesSummaryFile.
 01 SummaryRec.
    02 SalespsnNum PIC 9(5).
    02 TotalQtySold PIC 9(6).

 PROCEDURE DIVISION.
 Begin.
    SORT WorkFile ON ASCENDING KEY SalespsnNumWF
           USING SalesFile
           OUTPUT PROCEDURE IS SummariseSales
    STOP RUN.

 SummariseSales.
    OPEN OUTPUT SalesSummaryFile
    RETURN WorkFile
       AT END SET EndOfWorkFile TO TRUE
    END-RETURN
    PERFORM UNTIL EndOfWorkFile
      MOVE SalespsnNumWF TO SalespsnNum
      MOVE ZEROS TO TotalQtySold
      PERFORM UNTIL SalespsnNumWF NOT = SalespsnNum
                    OR EndOfWorkFile
         ADD QtySoldWF TO TotalQtySold
         RETURN WorkFile
            AT END SET EndOfWorkFile TO TRUE
         END-RETURN
      END-PERFORM
      WRITE SummaryRec
    END-PERFORM
    CLOSE SalesSummaryFile.