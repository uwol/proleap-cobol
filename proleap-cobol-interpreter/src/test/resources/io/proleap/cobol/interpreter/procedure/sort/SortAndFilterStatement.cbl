 IDENTIFICATION DIVISION.
 PROGRAM-ID. MaleSort.
 
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    SELECT PersonFile ASSIGN TO "PERSONS.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.

    SELECT MalePersonFile ASSIGN TO "MALEPERSONS.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.

    SELECT WorkFile ASSIGN TO "WORK.TMP".


 DATA DIVISION.
 FILE SECTION.
 FD PersonFile.
 01 PersonRec PIC X(30).
   88 EndOfFile VALUE HIGH-VALUES.

 FD MalePersonFile.
 01 MalePersonRec PIC X(30).

 SD WorkFile.
 01 WorkRec.
   02 FILLER PIC 9(7).
   02 WPersonName PIC X(10).
   02 FILLER PIC X(12).
   02 WGender PIC X.
      88 MalePerson VALUE "M".


 PROCEDURE DIVISION.
 Begin.
   SORT WorkFile ON ASCENDING KEY WPersonName
        INPUT PROCEDURE IS GetMalePersons
        GIVING MalePersonFile.
   STOP RUN.


 GetMalePersons.
   OPEN INPUT PersonFile
   READ PersonFile
      AT END SET EndOfFile TO TRUE
   END-READ
   PERFORM UNTIL EndOfFile
      MOVE PersonRec TO WorkRec
      IF MalePerson
         RELEASE WorkRec
      END-IF
      READ PersonFile
        AT END SET EndOfFile TO TRUE
      END-READ 
   END-PERFORM
   CLOSE PersonFile.