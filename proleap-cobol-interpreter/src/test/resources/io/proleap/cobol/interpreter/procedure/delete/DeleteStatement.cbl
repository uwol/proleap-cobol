 IDENTIFICATION DIVISION.
 PROGRAM-ID. SeqDelete.

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    SELECT PersonFile ASSIGN TO "PERSONS.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.

 DATA DIVISION.
 FILE SECTION.
 FD PersonFile.
 01 PersonDetails.
   02  PersonId PIC 9(7).
   02  PersonName.
       03 Lastname PIC X(8).
       03 Initials PIC XX.

 PROCEDURE DIVISION.
 Begin.
   OPEN INPUT PersonFile
   READ PersonFile
      AT END MOVE HIGH-VALUES TO PersonDetails
   END-READ
   DELETE PersonFile
   CLOSE PersonFile
   STOP RUN.