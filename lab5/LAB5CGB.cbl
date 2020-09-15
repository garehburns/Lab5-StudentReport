       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BKBRK.
       AUTHOR.         GARRETT BURNS.
      *
      *                    SINGLE-LEVEL CONTROL BREAK
      *
      *    THIS PROGRAM READS A STUDENT FILE.  
      *    YOU WILL NEED TO: 
      *
      *    INSERT A SINGLE-LEVEL CONTROL BREAK ON CLASS CODE
      *    PRINT THE CLASS-GROUP-LINE, 
      *    CREATE A NESTED IF STATEMENT TO EVALUATE THE STUDENT
      *    AVERAGE GRADE 
      *
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "STUDENT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "STUDENTCGB".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD STUDENT-FILE
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  STUDENT-RECORD.
           05  SR-DEPT-CODE                    PIC A(4).
           05  SR-CLASS-CODE                   PIC X(5).
           05  SR-NAME                         PIC X(20).
           05  SR-TEST1                        PIC 9(3).
           05  SR-TEST2                        PIC 9(3).
           05  SR-TEST3                        PIC 9(3).
           05  SR-TEST4                        PIC 9(3).


      *
       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-RECORD                     PIC X(80).

      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
           05  WS-CLASS-HOLD               PIC X(5).
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC S9      VALUE +1.
           05  PAGE-NO                     PIC S9(2)   VALUE +0.

      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)    VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
      *     05  DF-CLASS-HOLD                PIC X(5).
           05  DF-CLASS-TOTAL               PIC S9(5)    VALUE +0.
      *
       01  HEADING-ONE.
           05                              PIC X(6)  VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X     VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X     VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7)  VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(13) VALUE 'CGB'.
           05                              PIC X(5)  VALUE 'PAGE'.
           05  H1-PAGE-NO                  PIC Z9.
      *
       01  HEADING-TWO.
           05                              PIC X(5)  VALUE SPACES.
           05                              PIC X(20) VALUE
                                               'CLASS CODE  '.
           05                              PIC X(5)  VALUE SPACES.
           05 H2-CLASS-CODE                PIC X(5).
      *
       01  HEADING-THREE.
           05                              PIC X(19) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(3)  VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7)  VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                               PIC X(7) VALUE SPACES.
           05  DL-NAME                      PIC X(20).
           05                               PIC X(7).
           05  DL-TEST1                     PIC XXXBBBBB.
           05  DL-TEST2                     PIC XXXBBBBB.
           05  DL-TEST3                     PIC XXXBBBBB.
           05  DL-TEST4                     PIC XXXBBBBB.
           05  DL-GRADE                     PIC X.

      *
       01  CLASS-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL NUMBER OF STUDENTS FOR CLASS '.
           05  CGL-CLASS-CODE              PIC X(5).
           05                              PIC X(5)    VALUE ' IS  '.
           05  CGL-CLASS-TOTAL             PIC ZZZ9.


      *
       PROCEDURE DIVISION.
      *
       10-PRINT-STUDENT-REPORT.

           PERFORM 20-HSKPING-ROUTINE
           PERFORM 30-READ-STUDENT-FILE
           PERFORM 700-FINAL-ROUTINE
       .

       20-HSKPING-ROUTINE.

           OPEN INPUT  STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
           PERFORM 40-REPORT-HEADING
       .

       30-READ-STUDENT-FILE.

           PERFORM UNTIL EOF-FLAG = 'N'
               READ STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 100-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM
       .

       40-REPORT-HEADING.

           ADD 1 TO PAGE-NO
           MOVE PAGE-NO TO H1-PAGE-NO
           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE

           MOVE 2 TO PROPER-SPACING
       .

       100-PROCESS-STUDENT-RECORD.
       
      ************************************** vvv start of work
      *  CHECK FOR CONTROL BREAK HERE
      *  A NESTED IF 

           IF FIRST-RECORD = 'YES'
               PERFORM 400-PRINT-CLASS-HEADER
               MOVE 'NO' TO FIRST-RECORD
               
               MOVE SR-CLASS-CODE TO WS-CLASS-HOLD
               
           ELSE
               IF SR-CLASS-CODE NOT = WS-CLASS-HOLD
                   PERFORM 300-CLASS-BREAK
               END-IF
               
           END-IF
      
      ************************************** ^^^ end of work

           MOVE SR-NAME TO DL-NAME
           MOVE SR-TEST1 TO DL-TEST1
           MOVE SR-TEST2 TO DL-TEST2
           MOVE SR-TEST3 TO DL-TEST3
           MOVE SR-TEST4 TO DL-TEST4

           ADD SR-TEST1
               SR-TEST2
               SR-TEST3
               SR-TEST4 TO DF-TEST-TOTAL


           DIVIDE DF-TEST-TOTAL BY 4
                  GIVING DF-TEST-AVERAGE ROUNDED

      * USE NESTED IF STATEMENTS TO TEST DF-TEST-AVERAGE
      *  GREATER THAN 89 - A
      *  80 TO 89 - B
      *  70 TO 79 - C
      *  60 TO 69 - D
      *  LESS THAN 60 - F
      
      ************************************** vvv start of work
      
           IF DF-TEST-AVERAGE > 89
               MOVE 'A' TO DL-GRADE
           ELSE
               IF DF-TEST-AVERAGE >= 80 AND DF-TEST-AVERAGE <= 89
                   MOVE 'B' TO DL-GRADE
               ELSE
                   IF DF-TEST-AVERAGE >= 70 AND DF-TEST-AVERAGE <= 79
                       MOVE 'C' TO DL-GRADE
                   ELSE
                       IF DF-TEST-AVERAGE >= 60 AND DF-TEST-AVERAGE <= 69
                           MOVE 'D' TO DL-GRADE
                       ELSE
                           IF DF-TEST-AVERAGE < 60
                               MOVE 'F' TO DL-GRADE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
                               
      ************************************** ^^^ end of work

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           ADD 1 TO DF-CLASS-TOTAL

           MOVE ZEROS TO DF-TEST-AVERAGE
           MOVE ZEROS TO DF-TEST-TOTAL

           .
       200-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .

      *
       300-CLASS-BREAK.

      ************************************** vvv start of work
      
      *CODE FOR THE CONTROL BREAK GOES HERE
      
      *MOVE THE CLASS YOU WERE JUST WORKING WITH IN 100-PROCESS-STUDENT-RECORD TO CGL
         MOVE WS-CLASS-HOLD TO CGL-CLASS-CODE
         
      *MOVE THE NEW CLASS YOU WILL BE WORKING WITH TO HOLDING
         MOVE SR-CLASS-CODE TO WS-CLASS-HOLD

         MOVE DF-CLASS-TOTAL TO CGL-CLASS-TOTAL


         MOVE 2 TO PROPER-SPACING
         MOVE CLASS-GROUP-LINE TO REPORT-RECORD
         PERFORM 200-WRITE-A-LINE
         
      *NEED HEADERS FOR THE NEW GROUP/CLASS
         PERFORM 400-PRINT-CLASS-HEADER
         
         MOVE 0 TO DF-CLASS-TOTAL

      ************************************** ^^^ end of work
         .

       400-PRINT-CLASS-HEADER.

           MOVE SR-CLASS-CODE TO H2-CLASS-CODE

           WRITE REPORT-RECORD FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING

           WRITE REPORT-RECORD FROM HEADING-THREE
               AFTER ADVANCING PROPER-SPACING


           .
       500-END-OF-JOB-ROUTINE.
      *    CODE FOR LAST CONTROL LINE GOES HERE

           PERFORM 300-CLASS-BREAK

           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING





        .



       700-FINAL-ROUTINE.

           PERFORM 500-END-OF-JOB-ROUTINE

           CLOSE STUDENT-FILE
                 STUDENT-REPORT-FILE
            STOP RUN
            .

