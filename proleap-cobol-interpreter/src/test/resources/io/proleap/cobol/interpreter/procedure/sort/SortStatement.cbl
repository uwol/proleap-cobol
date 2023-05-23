 IDENTIFICATION DIVISION.
 PROGRAM-ID. InputSort.
 
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    SELECT PersonFile ASSIGN TO "SORTPERSON.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.
    SELECT WorkFile ASSIGN TO "WORK.TMP".

 DATA DIVISION.
 FILE SECTION.
 FD PersonFile.
 01 PersonDetails PIC X(25).

 SD WorkFile.
 01 WorkRec.
   02 WPersonId PIC 9(7).
   02 FILLER PIC X(18).

 PROCEDURE DIVISION.
 Begin.
   SORT WorkFile ON ASCENDING KEY WPersonId
        INPUT PROCEDURE IS GetPersonDetails
        GIVING PersonFile.
   STOP RUN.

 GetPersonDetails.
    DISPLAY "Enter person details using template below."
    DISPLAY "Enter - PersId, Lastname, Initials, YOB, MOB, DOB"
    DISPLAY "NNNNNNNPPPPPPPPIIYYYYMMDD"
    ACCEPT  WorkRec.
    PERFORM UNTIL WorkRec = SPACES
       RELEASE WorkRec
       ACCEPT WorkRec
    END-PERFORM.