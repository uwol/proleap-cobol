 IDENTIFICATION DIVISION.
 PROGRAM-ID. ReadWriteRecords.

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
      SELECT PersonRecords ASSIGN "PERSONS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT TransRecords ASSIGN "TRANSINS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

      SELECT NewPersonRecords ASSIGN "PERSONS.NEW"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

 DATA DIVISION.
 FILE SECTION.
 FD PersonRecords.
 01 PersonRecord.
   88 EndOfPersonFile     VALUE HIGH-VALUES.
   02 PersonID            PIC X(7).
   02 FILLER              PIC X(23).

 FD TransRecords.
 01 TransRecord.
   88 EndOfTransFile      VALUE HIGH-VALUES.
   02 TransPersonID       PIC X(7).
   02 FILLER              PIC X(23).

 FD NewPersonRecords.
 01 NewPersonRecord       PIC X(30).


 PROCEDURE DIVISION.
 BEGIN.
    OPEN INPUT PersonRecords
    OPEN INPUT TransRecords
    OPEN OUTPUT NewPersonRecords

    READ PersonRecords
       AT END SET EndOfPersonFile TO TRUE
    END-READ

    READ TransRecords
       AT END SET EndOfTransFile TO TRUE
    END-READ

    PERFORM UNTIL (EndOfPersonFile) AND (EndOfTransFile)
       EVALUATE TRUE
         WHEN (PersonID < TransPersonID)
              WRITE NewPersonRecord FROM PersonRecord
              READ PersonRecords
                 AT END SET EndOfPersonFile TO TRUE
              END-READ

         WHEN (PersonID > TransPersonID)
              WRITE NewPersonRecord FROM TransRecord
              READ TransRecords
                  AT END SET EndOfTransFile TO TRUE
              END-READ

         WHEN (PersonID = TransPersonID)
              DISPLAY "Error - " TransPersonId " already exists in file"
              READ TransRecords
                  AT END SET EndOfTransFile TO TRUE
              END-READ
       END-EVALUATE
    END-PERFORM
    
    CLOSE PersonRecords
    CLOSE TransRecords
    CLOSE NewPersonRecords
    STOP RUN.