 IDENTIFICATION DIVISION.
 PROGRAM-ID.  PerformTimes.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 TimesNum PIC 9 VALUE 3.

 PROCEDURE DIVISION.
 Begin.
    DISPLAY "Starting program"
    PERFORM SomeParagraph TimesNum TIMES
    DISPLAY "Stopping program".
    STOP RUN.

 SomeParagraph.
    DISPLAY "Paragraph perform".