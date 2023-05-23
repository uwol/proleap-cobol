 IDENTIFICATION DIVISION.
 PROGRAM-ID. SeqRead.

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
   02  DateOfBirth.
       03 YOBirth PIC 9(4).
       03 MOBirth PIC 9(2).
       03 DOBirth PIC 9(2).

 PROCEDURE DIVISION.
 Begin.
   OPEN INPUT PersonFile
   READ PersonFile
      AT END MOVE HIGH-VALUES TO PersonDetails
   END-READ
   PERFORM UNTIL PersonDetails = HIGH-VALUES
      DISPLAY PersonId SPACE PersonName SPACE YOBirth
      READ PersonFile
         AT END MOVE HIGH-VALUES TO PersonDetails
      END-READ
   END-PERFORM
   CLOSE PersonFile
   STOP RUN.