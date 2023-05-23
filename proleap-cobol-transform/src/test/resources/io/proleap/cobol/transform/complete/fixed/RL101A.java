import java.lang.String;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import javax.inject.Inject;
import javax.inject.Singleton;

import javax.validation.constraints.*;
import io.proleap.cobol.api.*;
import io.proleap.cobol.api.data.*;
import io.proleap.cobol.api.environment.configuration.object.*;
import io.proleap.cobol.api.environment.configuration.source.*;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.*;
import io.proleap.cobol.api.ProLeapCobolApiSpringConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

@Singleton
public class RL101A {
    /**                                                                                              //   (2)        IDENTIFICATION DIVISION.
    * Program-Id: RL101A                                                                             //   (3)        PROGRAM-ID.
    */
                                                                                                     //   (4)            RL101A.
                                                                                                     //   (5)       *> ***************************************************************
                                                                                                     //   (6)       *>                                                               *
                                                                                                     //   (7)       *>     VALIDATION FOR:-                                          *
                                                                                                     //   (8)       *>                                                               *
                                                                                                     //   (9)       *>     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
                                                                                                     //  (10)       *>                                                               *
                                                                                                     //  (11)       *>     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
                                                                                                     //  (12)       *>                                                               *
                                                                                                     //  (13)       *> ***************************************************************
                                                                                                     //  (14)       *>                                                               *
                                                                                                     //  (15)       *>       X-CARDS USED BY THIS PROGRAM ARE :-                     *
                                                                                                     //  (16)       *>                                                               *
                                                                                                     //  (17)       *>             X-21   IMPLEMENTOR-NAME IN ASSIGN TO CLAUSE FOR   *
                                                                                                     //  (18)       *>                     RELATIVE  I-O DATA FILE                   *
                                                                                                     //  (19)       *>             X-55   SYSTEM PRINTER                             *
                                                                                                     //  (20)       *>             X-69   ADDITIONAL VALUE OF CLAUSES                *
                                                                                                     //  (21)       *>             X-74   VALUE OF IMPLEMENTOR-NAME                  *
                                                                                                     //  (22)       *>             X-75   OBJECT OF VALUE OF CLAUSE                  *
                                                                                                     //  (23)       *>             X-82   SOURCE-COMPUTER                            *
                                                                                                     //  (24)       *>             X-83   OBJECT-COMPUTER.                           *
                                                                                                     //  (25)       *>                                                               *
                                                                                                     //  (26)       *> ***************************************************************
                                                                                                     //  (27)       *>    RL111A                                                     *
                                                                                                     //  (28)       *> ***************************************************************
                                                                                                     //  (29)       *>                                                               *
                                                                                                     //  (30)       *>      THIS PROGRAM WILL TEST THE NEW SYNTACTICAL CONSTRUCTS    *
                                                                                                     //  (31)       *>      AND SEMENTIC ACTIONS ASSOCIATED WITH THE FOLLOWING       *
                                                                                                     //  (32)       *>      CLAUSES:                                                 *
                                                                                                     //  (33)       *>           - ACCESS                                            *
                                                                                                     //  (34)       *>           - READ                                              *
                                                                                                     //  (35)       *>           - WRITE                                             *
                                                                                                     //  (36)       *>           - REWRITE                                           *
                                                                                                     //  (37)       *>                                                               *
                                                                                                     //  (38)       *>      1) THE PROGRAM WILL CREATE A RELATIVE I-O FILE           *
                                                                                                     //  (39)       *>      2) THEN IT WILL UPDATE SELECTIVE RECORDS OF THE FILE     *
                                                                                                     //  (40)       *>      3) THE FILE STATUS CONTENTS ARE CAPTURED AND TESTED FOR  *
                                                                                                     //  (41)       *>         ACCURACY FOR EACH "OPEN", "CLOSE", "READ" AND         *
                                                                                                     //  (42)       *>         "REWRITE" STATEMENT USED.                             *
                                                                                                     //  (43)       *>      4) THE "READ", "WRITE" AND "REWRITE" STATEMENT WILL BE   *
                                                                                                     //  (44)       *>         USED WITH THE APPROPRIATE "AT END", "NOT AT END",     *
                                                                                                     //  (45)       *>         "INVALID KEY" AND "NOT INVALID KEY" PHRASES.          *
                                                                                                     //  (46)       *>                                                               *
                                                                                                     //  (47)       *> ***************************************************************
                                                                                                     //  (48)        ENVIRONMENT DIVISION.
                                                                                                     //  (49)        CONFIGURATION SECTION.
    SourceComputer xxxxx082SourceComputer = new SourceComputer();                                    //  (50)        SOURCE-COMPUTER.
                                                                                                     //  (51)            XXXXX082.
    ObjectComputer xxxxx083ObjectComputer = new ObjectComputer();                                    //  (52)        OBJECT-COMPUTER.
                                                                                                     //  (53)            XXXXX083.
                                                                                                     //  (54)        INPUT-OUTPUT SECTION.
    @Inject                                                                                          //  (55)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry print_file = new FileControlEntry("xxxxx055");                                  //  (56)            SELECT PRINT-FILE ASSIGN TO
                                                                                                     //  (57)            XXXXX055.
    FileControlEntry rl_fs2 = new FileControlEntry("xxxxp021");                                      //  (58)            SELECT   RL-FS2 ASSIGN TO
                                                                                                     //  (59)            XXXXP021
                                                                                                     //  (60)                    ORGANIZATION IS RELATIVE
                                                                                                     //  (61)                    ACCESS       IS SEQUENTIAL
                                                                                                     //  (62)                    STATUS          RL-FS2-STATUS.
    @Inject                                                                                          //  (63)        DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //  (64)        FILE SECTION.
    public class Print_fileType {                                                                    //  (65)        FD  PRINT-FILE.
        @Size(max=120)                                                                               //  (66)        01  PRINT-REC PICTURE X(120).
        protected String print_rec;                                                                 
        @Size(max=120)                                                                               //  (67)        01  DUMMY-RECORD PICTURE X(120).
        protected String dummy_record;                                                              
    }
    public Print_fileType print_fileContent = new Print_fileType();
    public class Rl_fs2Type {                                                                        //  (68)        FD  RL-FS2
                                                                                                     //  (69)            LABEL RECORDS STANDARD
                                                                                                     //  (70)            VALUE OF
                                                                                                     //  (71)            XXXXX074
                                                                                                     //  (72)            IS
                                                                                                     //  (73)            XXXXX075
                                                                                                     //  (74)       *>     XXXXX069
                                                                                                     //  (75)            BLOCK CONTAINS 1 RECORDS
                                                                                                     //  (76)            RECORD CONTAINS 120 CHARACTERS.
        public class Rl_fs2r1_f_g_120Type {                                                          //  (77)        01  RL-FS2R1-F-G-120.
            @Size(max=120)                                                                           //  (78)            02 FILLER PIC X(120).
            protected String filler0;                                                               
        }
        
        protected Rl_fs2r1_f_g_120Type rl_fs2r1_f_g_120 = new Rl_fs2r1_f_g_120Type();
    }
    public Rl_fs2Type rl_fs2Content = new Rl_fs2Type();
                                                                                                     //  (79)        WORKING-STORAGE SECTION.
    @Size(max=2)                                                                                     //  (80)        01  RL-FS2-STATUS         PIC XX.
    protected String rl_fs2_status;                                                                 
    @Digits(integer=9, fraction=0)                                                                   //  (81)        01  WRK-CS-09V00 PIC S9(9) USAGE COMP VALUE ZERO.
    protected BigDecimal wrk_cs_09v00 = BigDecimal.ZERO;                                            
    public class File_record_information_recType {                                                   //  (82)        01  FILE-RECORD-INFORMATION-REC.
        public class File_record_info_skeletonType {                                                 //  (83)            03 FILE-RECORD-INFO-SKELETON.
            @Size(max=48)                                                                            //  (84)               05 FILLER                 PICTURE X(48)       VALUE
            protected String filler1 = "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00";          
                                                                                                     //  (85)                    "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00".
            @Size(max=46)                                                                            //  (86)               05 FILLER                 PICTURE X(46)       VALUE
            protected String filler2 = ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000";            
                                                                                                     //  (87)                    ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000".
            @Size(max=26)                                                                            //  (88)               05 FILLER                 PICTURE X(26)       VALUE
            protected String filler3 = ",LFIL=000000,ORG=  ,LBLR= ";                                
                                                                                                     //  (89)                    ",LFIL=000000,ORG=  ,LBLR= ".
            @Size(max=37)                                                                            //  (90)               05 FILLER                 PICTURE X(37)       VALUE
            protected String filler4 = ",RECKEY=                             ";                     
                                                                                                     //  (91)                    ",RECKEY=                             ".
            @Size(max=38)                                                                            //  (92)               05 FILLER                 PICTURE X(38)       VALUE
            protected String filler5 = ",ALTKEY1=                             ";                    
                                                                                                     //  (93)                    ",ALTKEY1=                             ".
            @Size(max=38)                                                                            //  (94)               05 FILLER                 PICTURE X(38)       VALUE
            protected String filler6 = ",ALTKEY2=                             ";                    
                                                                                                     //  (95)                    ",ALTKEY2=                             ".
            @Size(max=7)                                                                             //  (96)               05 FILLER                 PICTURE X(7)        VALUE SPACE.
            protected String filler7 = " ";                                                         
        }
        
        protected File_record_info_skeletonType file_record_info_skeleton = new File_record_info_skeletonType();
        public class File_record_infoType {                                                          //  (97)            03 FILE-RECORD-INFO          OCCURS  10  TIMES.
            public class File_record_info_p1_120Type {                                               //  (98)               05 FILE-RECORD-INFO-P1-120.
                @Size(max=5)                                                                         //  (99)                  07 FILLER              PIC X(5).
                protected String filler8;                                                           
                @Size(max=6)                                                                         // (100)                  07 XFILE-NAME           PIC X(6).
                protected String xfile_name;                                                        
                @Size(max=8)                                                                         // (101)                  07 FILLER              PIC X(8).
                protected String filler9;                                                           
                @Size(max=6)                                                                         // (102)                  07 XRECORD-NAME         PIC X(6).
                protected String xrecord_name;                                                      
                @Size(max=1)                                                                         // (103)                  07 FILLER              PIC X(1).
                protected String filler10;                                                          
                @Digits(integer=1, fraction=0)                                                       // (104)                  07 REELUNIT-NUMBER     PIC 9(1).
                protected BigDecimal reelunit_number;                                               
                @Size(max=7)                                                                         // (105)                  07 FILLER              PIC X(7).
                protected String filler11;                                                          
                @Digits(integer=6, fraction=0)                                                       // (106)                  07 XRECORD-NUMBER       PIC 9(6).
                protected BigDecimal xrecord_number;                                                
                @Size(max=6)                                                                         // (107)                  07 FILLER              PIC X(6).
                protected String filler12;                                                          
                @Digits(integer=2, fraction=0)                                                       // (108)                  07 UPDATE-NUMBER       PIC 9(2).
                protected BigDecimal update_number;                                                 
                @Size(max=5)                                                                         // (109)                  07 FILLER              PIC X(5).
                protected String filler13;                                                          
                @Digits(integer=4, fraction=0)                                                       // (110)                  07 ODO-NUMBER          PIC 9(4).
                protected BigDecimal odo_number;                                                    
                @Size(max=5)                                                                         // (111)                  07 FILLER              PIC X(5).
                protected String filler14;                                                          
                @Size(max=5)                                                                         // (112)                  07 XPROGRAM-NAME        PIC X(5).
                protected String xprogram_name;                                                     
                @Size(max=7)                                                                         // (113)                  07 FILLER              PIC X(7).
                protected String filler15;                                                          
                @Digits(integer=6, fraction=0)                                                       // (114)                  07 XRECORD-LENGTH       PIC 9(6).
                protected BigDecimal xrecord_length;                                                
                @Size(max=7)                                                                         // (115)                  07 FILLER              PIC X(7).
                protected String filler16;                                                          
                @Size(max=2)                                                                         // (116)                  07 CHARS-OR-RECORDS    PIC X(2).
                protected String chars_or_records;                                                  
                @Size(max=1)                                                                         // (117)                  07 FILLER              PIC X(1).
                protected String filler17;                                                          
                @Digits(integer=4, fraction=0)                                                       // (118)                  07 XBLOCK-SIZE          PIC 9(4).
                protected BigDecimal xblock_size;                                                   
                @Size(max=6)                                                                         // (119)                  07 FILLER              PIC X(6).
                protected String filler18;                                                          
                @Digits(integer=6, fraction=0)                                                       // (120)                  07 RECORDS-IN-FILE     PIC 9(6).
                protected BigDecimal records_in_file;                                               
                @Size(max=5)                                                                         // (121)                  07 FILLER              PIC X(5).
                protected String filler19;                                                          
                @Size(max=2)                                                                         // (122)                  07 XFILE-ORGANIZATION   PIC X(2).
                protected String xfile_organization;                                                
                @Size(max=6)                                                                         // (123)                  07 FILLER              PIC X(6).
                protected String filler20;                                                          
                @Size(max=1)                                                                         // (124)                  07 XLABEL-TYPE          PIC X(1).
                protected String xlabel_type;                                                       
            }
            
            protected File_record_info_p1_120Type file_record_info_p1_120 = new File_record_info_p1_120Type();
            public class File_record_info_p121_240Type {                                             // (125)               05 FILE-RECORD-INFO-P121-240.
                @Size(max=8)                                                                         // (126)                  07 FILLER              PIC X(8).
                protected String filler21;                                                          
                @Size(max=29)                                                                        // (127)                  07 XRECORD-KEY          PIC X(29).
                protected String xrecord_key;                                                       
                @Size(max=9)                                                                         // (128)                  07 FILLER              PIC X(9).
                protected String filler22;                                                          
                @Size(max=29)                                                                        // (129)                  07 ALTERNATE-KEY1      PIC X(29).
                protected String alternate_key1;                                                    
                @Size(max=9)                                                                         // (130)                  07 FILLER              PIC X(9).
                protected String filler23;                                                          
                @Size(max=29)                                                                        // (131)                  07 ALTERNATE-KEY2      PIC X(29).
                protected String alternate_key2;                                                    
                @Size(max=7)                                                                         // (132)                  07 FILLER              PIC X(7).
                protected String filler24;                                                          
            }
            
            protected File_record_info_p121_240Type file_record_info_p121_240 = new File_record_info_p121_240Type();
        }
        
        protected List<File_record_infoType> file_record_info = new ArrayList<File_record_infoType>();
        {
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
            file_record_info.add(new File_record_infoType());
        }
    }
    
    protected File_record_information_recType file_record_information_rec = new File_record_information_recType();
    public class Test_resultsType {                                                                  // (133)        01  TEST-RESULTS.
        @Size(max=1)                                                                                 // (134)            02 FILLER                   PIC X      VALUE SPACE.
        protected String filler25 = " ";                                                            
        @Size(max=20)                                                                                // (135)            02 FEATURE                  PIC X(20)  VALUE SPACE.
        protected String feature = " ";                                                             
        @Size(max=1)                                                                                 // (136)            02 FILLER                   PIC X      VALUE SPACE.
        protected String filler26 = " ";                                                            
        @Size(max=5)                                                                                 // (137)            02 P-OR-F                   PIC X(5)   VALUE SPACE.
        protected String p_or_f = " ";                                                              
        @Size(max=1)                                                                                 // (138)            02 FILLER                   PIC X      VALUE SPACE.
        protected String filler27 = " ";                                                            
        public class Par_nameType {                                                                  // (139)            02  PAR-NAME.
            @Size(max=19)                                                                            // (140)              03 FILLER                 PIC X(19)  VALUE SPACE.
            protected String filler28 = " ";                                                        
            @Size(max=1)                                                                             // (141)              03  PARDOT-X              PIC X      VALUE SPACE.
            protected String pardot_x = " ";                                                        
            @Digits(integer=2, fraction=0)                                                           // (142)              03 DOTVALUE               PIC 99     VALUE ZERO.
            protected BigDecimal dotvalue = BigDecimal.ZERO;                                        
        }
        
        protected Par_nameType par_name = new Par_nameType();
        @Size(max=8)                                                                                 // (143)            02 FILLER                   PIC X(8)   VALUE SPACE.
        protected String filler29 = " ";                                                            
        @Size(max=61)                                                                                // (144)            02 RE-MARK                  PIC X(61).
        protected String re_mark;                                                                   
    }
    
    protected Test_resultsType test_results = new Test_resultsType();
    public class Test_computedType {                                                                 // (145)        01  TEST-COMPUTED.
        @Size(max=30)                                                                                // (146)            02 FILLER                   PIC X(30)  VALUE SPACE.
        protected String filler30 = " ";                                                            
        @Size(max=17)                                                                                // (147)            02 FILLER                   PIC X(17)  VALUE
        protected String filler31 = "       COMPUTED=";                                             
                                                                                                     // (148)                   "       COMPUTED=".
        public class Computed_xType {                                                                // (149)            02 COMPUTED-X.
            @Size(max=20)                                                                            // (150)            03 COMPUTED-A               PIC X(20)  VALUE SPACE.
            protected String computed_a = " ";                                                      
            protected Object computed_n;                                                             // (151)            03 COMPUTED-N               REDEFINES COMPUTED-A
                                                                                                     // (152)                                        PIC -9(9).9(9).
            protected Object computed_0v18;                                                          // (153)            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).
            protected Object computed_4v14;                                                          // (154)            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).
            protected Object computed_14v4;                                                          // (155)            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).
            public class Cm_18v0Type {                                                               // (156)            03       CM-18V0 REDEFINES COMPUTED-A.
                protected Object computed_18v0;                                                      // (157)                04 COMPUTED-18V0                    PIC -9(18).
                @Size(max=1)                                                                         // (158)                04 FILLER                           PIC X.
                protected String filler32;                                                          
            }
            
            protected Cm_18v0Type cm_18v0 = new Cm_18v0Type();
            @Size(max=50)                                                                            // (159)            03 FILLER PIC X(50) VALUE SPACE.
            protected String filler33 = " ";                                                        
        }
        
        protected Computed_xType computed_x = new Computed_xType();
    }
    
    protected Test_computedType test_computed = new Test_computedType();
    public class Test_correctType {                                                                  // (160)        01  TEST-CORRECT.
        @Size(max=30)                                                                                // (161)            02 FILLER PIC X(30) VALUE SPACE.
        protected String filler34 = " ";                                                            
        @Size(max=17)                                                                                // (162)            02 FILLER PIC X(17) VALUE "       CORRECT =".
        protected String filler35 = "       CORRECT =";                                             
        public class Correct_xType {                                                                 // (163)            02 CORRECT-X.
            @Size(max=20)                                                                            // (164)            03 CORRECT-A                  PIC X(20) VALUE SPACE.
            protected String correct_a = " ";                                                       
            protected Object correct_n;                                                              // (165)            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).
            protected Object correct_0v18;                                                           // (166)            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).
            protected Object correct_4v14;                                                           // (167)            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).
            protected Object correct_14v4;                                                           // (168)            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).
            public class Cr_18v0Type {                                                               // (169)            03      CR-18V0 REDEFINES CORRECT-A.
                protected Object correct_18v0;                                                       // (170)                04 CORRECT-18V0                     PIC -9(18).
                @Size(max=1)                                                                         // (171)                04 FILLER                           PIC X.
                protected String filler36;                                                          
            }
            
            protected Cr_18v0Type cr_18v0 = new Cr_18v0Type();
            @Size(max=2)                                                                             // (172)            03 FILLER PIC X(2) VALUE SPACE.
            protected String filler37 = " ";                                                        
            @Size(max=48)                                                                            // (173)            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.
            protected String cor_ansi_reference = " ";                                              
        }
        
        protected Correct_xType correct_x = new Correct_xType();
    }
    
    protected Test_correctType test_correct = new Test_correctType();
    public class Ccvs_c_1Type {                                                                      // (174)        01  CCVS-C-1.
        @Size(max=99)                                                                                // (175)            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".
        protected String filler38 = " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS";
        @Size(max=20)                                                                                // (176)            02 FILLER                     PIC X(20)    VALUE SPACE.
        protected String filler39 = " ";                                                            
    }
    
    protected Ccvs_c_1Type ccvs_c_1 = new Ccvs_c_1Type();
    public class Ccvs_c_2Type {                                                                      // (177)        01  CCVS-C-2.
        @Size(max=1)                                                                                 // (178)            02 FILLER                     PIC X        VALUE SPACE.
        protected String filler40 = " ";                                                            
        @Size(max=6)                                                                                 // (179)            02 FILLER                     PIC X(6)     VALUE "TESTED".
        protected String filler41 = "TESTED";                                                       
        @Size(max=15)                                                                                // (180)            02 FILLER                     PIC X(15)    VALUE SPACE.
        protected String filler42 = " ";                                                            
        @Size(max=4)                                                                                 // (181)            02 FILLER                     PIC X(4)     VALUE "FAIL".
        protected String filler43 = "FAIL";                                                         
        @Size(max=94)                                                                                // (182)            02 FILLER                     PIC X(94)    VALUE SPACE.
        protected String filler44 = " ";                                                            
    }
    
    protected Ccvs_c_2Type ccvs_c_2 = new Ccvs_c_2Type();
    @Digits(integer=2, fraction=0)                                                                   // (183)        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.
    protected BigDecimal rec_skl_sub = BigDecimal.ZERO;                                             
    @Digits(integer=2, fraction=0)                                                                   // (184)        01  REC-CT                        PIC 99       VALUE ZERO.
    protected BigDecimal rec_ct = BigDecimal.ZERO;                                                  
    @Digits(integer=3, fraction=0)                                                                   // (185)        01  DELETE-COUNTER                PIC 999      VALUE ZERO.
    protected BigDecimal delete_counter = BigDecimal.ZERO;                                          
    @Digits(integer=3, fraction=0)                                                                   // (186)        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.
    protected BigDecimal error_counter = BigDecimal.ZERO;                                           
    @Digits(integer=3, fraction=0)                                                                   // (187)        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.
    protected BigDecimal inspect_counter = BigDecimal.ZERO;                                         
    @Digits(integer=3, fraction=0)                                                                   // (188)        01  PASS-COUNTER                  PIC 999      VALUE ZERO.
    protected BigDecimal pass_counter = BigDecimal.ZERO;                                            
    @Digits(integer=3, fraction=0)                                                                   // (189)        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.
    protected BigDecimal total_error = BigDecimal.ZERO;                                             
    @Digits(integer=3, fraction=0)                                                                   // (190)        01  ERROR-HOLD                    PIC 999      VALUE ZERO.
    protected BigDecimal error_hold = BigDecimal.ZERO;                                              
    @Size(max=120)                                                                                   // (191)        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.
    protected String dummy_hold = " ";                                                              
    @Digits(integer=5, fraction=0)                                                                   // (192)        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.
    protected BigDecimal record_count = BigDecimal.ZERO;                                            
    @Size(max=48)                                                                                    // (193)        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.
    protected String ansi_reference = " ";                                                          
    public class Ccvs_h_1Type {                                                                      // (194)        01  CCVS-H-1.
        @Size(max=39)                                                                                // (195)            02  FILLER                    PIC X(39)    VALUE SPACES.
        protected String filler45 = " ";                                                            
        @Size(max=42)                                                                                // (196)            02  FILLER                    PIC X(42)    VALUE
        protected String filler46 = "OFFICIAL COBOL COMPILER VALIDATION SYSTEM";                    
                                                                                                     // (197)            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".
        @Size(max=39)                                                                                // (198)            02  FILLER                    PIC X(39)    VALUE SPACES.
        protected String filler47 = " ";                                                            
    }
    
    protected Ccvs_h_1Type ccvs_h_1 = new Ccvs_h_1Type();
    public class Ccvs_h_2aType {                                                                     // (199)        01  CCVS-H-2A.
        @Size(max=40)                                                                                // (200)          02  FILLER                        PIC X(40)  VALUE SPACE.
        protected String filler48 = " ";                                                            
        @Size(max=7)                                                                                 // (201)          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".
        protected String filler49 = "CCVS85 ";                                                      
        @Size(max=4)                                                                                 // (202)          02  FILLER                        PIC XXXX   VALUE
        protected String filler50 = "4.2 ";                                                         
                                                                                                     // (203)            "4.2 ".
        @Size(max=28)                                                                                // (204)          02  FILLER                        PIC X(28)  VALUE
        protected String filler51 = " COPY - NOT FOR DISTRIBUTION";                                 
                                                                                                     // (205)                   " COPY - NOT FOR DISTRIBUTION".
        @Size(max=41)                                                                                // (206)          02  FILLER                        PIC X(41)  VALUE SPACE.
        protected String filler52 = " ";                                                            
    }
    
    protected Ccvs_h_2aType ccvs_h_2a = new Ccvs_h_2aType();
                                                                                                     // (207)        
    public class Ccvs_h_2bType {                                                                     // (208)        01  CCVS-H-2B.
        @Size(max=15)                                                                                // (209)          02  FILLER                        PIC X(15)  VALUE
        protected String filler53 = "TEST RESULT OF ";                                              
                                                                                                     // (210)                   "TEST RESULT OF ".
        @Size(max=9)                                                                                 // (211)          02  TEST-ID                       PIC X(9).
        protected String test_id;                                                                   
        @Size(max=4)                                                                                 // (212)          02  FILLER                        PIC X(4)   VALUE
        protected String filler54 = " IN ";                                                         
                                                                                                     // (213)                   " IN ".
        @Size(max=12)                                                                                // (214)          02  FILLER                        PIC X(12)  VALUE
        protected String filler55 = " HIGH       ";                                                 
                                                                                                     // (215)            " HIGH       ".
        @Size(max=22)                                                                                // (216)          02  FILLER                        PIC X(22)  VALUE
        protected String filler56 = " LEVEL VALIDATION FOR ";                                       
                                                                                                     // (217)                   " LEVEL VALIDATION FOR ".
        @Size(max=58)                                                                                // (218)          02  FILLER                        PIC X(58)  VALUE
        protected String filler57 = "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ";   
    }
    
    protected Ccvs_h_2bType ccvs_h_2b = new Ccvs_h_2bType();
                                                                                                     // (219)            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
    public class Ccvs_h_3Type {                                                                      // (220)        01  CCVS-H-3.
        @Size(max=34)                                                                                // (221)            02  FILLER                      PIC X(34)  VALUE
        protected String filler58 = " FOR OFFICIAL USE ONLY    ";                                   
                                                                                                     // (222)                   " FOR OFFICIAL USE ONLY    ".
        @Size(max=58)                                                                                // (223)            02  FILLER                      PIC X(58)  VALUE
        protected String filler59 = "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ";   
                                                                                                     // (224)            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
        @Size(max=28)                                                                                // (225)            02  FILLER                      PIC X(28)  VALUE
        protected String filler60 = "  COPYRIGHT   1985 ";                                          
    }
    
    protected Ccvs_h_3Type ccvs_h_3 = new Ccvs_h_3Type();
                                                                                                     // (226)                   "  COPYRIGHT   1985 ".
    public class Ccvs_e_1Type {                                                                      // (227)        01  CCVS-E-1.
        @Size(max=52)                                                                                // (228)            02 FILLER                       PIC X(52)  VALUE SPACE.
        protected String filler61 = " ";                                                            
        @Size(max=14)                                                                                // (229)            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".
        protected String filler62 = "END OF TEST-  ";                                               
        @Size(max=9)                                                                                 // (230)            02 ID-AGAIN                     PIC X(9).
        protected String id_again;                                                                  
        @Size(max=45)                                                                                // (231)            02 FILLER                       PIC X(45)  VALUE SPACES.
        protected String filler63 = " ";                                                            
    }
    
    protected Ccvs_e_1Type ccvs_e_1 = new Ccvs_e_1Type();
    public class Ccvs_e_2Type {                                                                      // (232)        01  CCVS-E-2.
        @Size(max=31)                                                                                // (233)            02  FILLER                      PIC X(31)  VALUE SPACE.
        protected String filler64 = " ";                                                            
        @Size(max=21)                                                                                // (234)            02  FILLER                      PIC X(21)  VALUE SPACE.
        protected String filler65 = " ";                                                            
        public class Ccvs_e_2_2Type {                                                                // (235)            02 CCVS-E-2-2.
            @Size(max=3)                                                                             // (236)                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.
            protected String error_total = " ";                                                     
            @Size(max=1)                                                                             // (237)                03 FILLER                   PIC X      VALUE SPACE.
            protected String filler66 = " ";                                                        
            @Size(max=44)                                                                            // (238)                03 ENDER-DESC               PIC X(44)  VALUE
            protected String ender_desc = "ERRORS ENCOUNTERED";                                     
        }
        
        protected Ccvs_e_2_2Type ccvs_e_2_2 = new Ccvs_e_2_2Type();
    }
    
    protected Ccvs_e_2Type ccvs_e_2 = new Ccvs_e_2Type();
                                                                                                     // (239)                   "ERRORS ENCOUNTERED".
    public class Ccvs_e_3Type {                                                                      // (240)        01  CCVS-E-3.
        @Size(max=22)                                                                                // (241)            02  FILLER                      PIC X(22)  VALUE
        protected String filler67 = " FOR OFFICIAL USE ONLY";                                       
                                                                                                     // (242)                   " FOR OFFICIAL USE ONLY".
        @Size(max=12)                                                                                // (243)            02  FILLER                      PIC X(12)  VALUE SPACE.
        protected String filler68 = " ";                                                            
        @Size(max=58)                                                                                // (244)            02  FILLER                      PIC X(58)  VALUE
        protected String filler69 = "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ";   
                                                                                                     // (245)            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        @Size(max=13)                                                                                // (246)            02  FILLER                      PIC X(13)  VALUE SPACE.
        protected String filler70 = " ";                                                            
        @Size(max=15)                                                                                // (247)            02 FILLER                       PIC X(15)  VALUE
        protected String filler71 = " COPYRIGHT 1985";                                              
    }
    
    protected Ccvs_e_3Type ccvs_e_3 = new Ccvs_e_3Type();
                                                                                                     // (248)                    " COPYRIGHT 1985".
    public class Ccvs_e_4Type {                                                                      // (249)        01  CCVS-E-4.
        @Size(max=3)                                                                                 // (250)            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.
        protected String ccvs_e_4_1 = " ";                                                          
        @Size(max=4)                                                                                 // (251)            02 FILLER                       PIC X(4)   VALUE " OF ".
        protected String filler72 = " OF ";                                                         
        @Size(max=3)                                                                                 // (252)            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.
        protected String ccvs_e_4_2 = " ";                                                          
        @Size(max=40)                                                                                // (253)            02 FILLER                       PIC X(40)  VALUE
        protected String filler73 = "  TESTS WERE EXECUTED SUCCESSFULLY";                           
    }
    
    protected Ccvs_e_4Type ccvs_e_4 = new Ccvs_e_4Type();
                                                                                                     // (254)             "  TESTS WERE EXECUTED SUCCESSFULLY".
    public class XXINFOType {                                                                        // (255)        01  XXINFO.
        @Size(max=19)                                                                                // (256)            02 FILLER                       PIC X(19)  VALUE
        protected String filler74 = "*** INFORMATION ***";                                          
                                                                                                     // (257)                   "*** INFORMATION ***".
        public class Info_textType {                                                                 // (258)            02 INFO-TEXT.
            @Size(max=8)                                                                             // (259)              04 FILLER                     PIC X(8)   VALUE SPACE.
            protected String filler75 = " ";                                                        
            @Size(max=20)                                                                            // (260)              04 XXCOMPUTED                 PIC X(20).
            protected String xxcomputed;                                                            
            @Size(max=5)                                                                             // (261)              04 FILLER                     PIC X(5)   VALUE SPACE.
            protected String filler76 = " ";                                                        
            @Size(max=20)                                                                            // (262)              04 XXCORRECT                  PIC X(20).
            protected String xxcorrect;                                                             
        }
        
        protected Info_textType info_text = new Info_textType();
        @Size(max=48)                                                                                // (263)            02 INF-ANSI-REFERENCE           PIC X(48).
        protected String inf_ansi_reference;                                                        
    }
    
    protected XXINFOType xxinfo = new XXINFOType();
    public class Hyphen_lineType {                                                                   // (264)        01  HYPHEN-LINE.
        @Size(max=1)                                                                                 // (265)            02 FILLER  PIC IS X VALUE IS SPACE.
        protected String filler77 = " ";                                                            
        @Size(max=65)                                                                                // (266)            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".
        protected String filler78 = "*****************************************************************";
        @Size(max=54)                                                                                // (267)            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".
        protected String filler79 = "******************************************************";       
    }
    
    protected Hyphen_lineType hyphen_line = new Hyphen_lineType();
    @Size(max=9)                                                                                     // (268)        01  CCVS-PGM-ID                     PIC X(9)   VALUE
    protected String ccvs_pgm_id = "RL101A";                                                        
                                                                                                     // (269)            "RL101A".
                                                                                                     // (270)        PROCEDURE DIVISION.
    public void ccvs1() throws Exception {                                                           // (271)        CCVS1 SECTION.
        open_files();
        ccvs_init_file();
        ccvs_init_exit();
        close_files();
        terminate_ccvs();
        inspt();
        pass();
        fail();
        de_lete();
        print_detail();
        head_routine();
        column_names_routine();
        end_routine();
        end_rtn_exit();
        end_routine_1();
        end_routine_12();
        end_routine_13();
        write_line();
        wrt_ln();
        blank_line_print();
        fail_routine();
        fail_routine_write();
        fail_routine_ex();
        bail_out();
        bail_out_write();
        bail_out_ex();
        ccvs1_exit();
    }
    
    public void open_files() throws Exception {                                                      // (272)        OPEN-FILES.
        fileControlService.openOutput(print_file);                                                   // (273)            OPEN    OUTPUT PRINT-FILE.
        test_id.ccvs_h_2b = ccvs_pgm_id;                                                             // (274)            MOVE  CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.
        id_again.ccvs_e_1 = ccvs_pgm_id;                                                            
        entityService.assignTo(test_results, " ");                                                   // (275)            MOVE    SPACE TO TEST-RESULTS.
        head_routine();                                                                              // (276)            PERFORM HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.
        column_names_routine();                                                                     
        rec_skl_sub = BigDecimal.ZERO;                                                               // (277)            MOVE    ZERO TO REC-SKL-SUB.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(9)) < 0; i = i.add(BigDecimal.ONE)){
            ccvs_init_file();                                                                        // (278)            PERFORM CCVS-INIT-FILE 9 TIMES.
        }                                                                                           
                                                                                                    
    }
    
    public void ccvs_init_file() throws Exception {                                                  // (279)        CCVS-INIT-FILE.
        rec_skl_sub = rec_skl_sub.add(BigDecimal.ONE);                                               // (280)            ADD     1 TO REC-SKL-SUB.
                                                                                                     // (281)            MOVE    FILE-RECORD-INFO-SKELETON
        entityService.assignTo(file_record_info.get(rec_skl_sub.intValue() - 1).file_record_information_rec, file_record_info_skeleton.file_record_information_rec); // (282)                 TO FILE-RECORD-INFO (REC-SKL-SUB).
    }
    
    public void ccvs_init_exit() throws Exception {                                                  // (283)        CCVS-INIT-EXIT.
        ccvs1_exit();                                                                                // (284)            GO TO CCVS1-EXIT.
        throw new RuntimeException("ccvs1_exit must terminate due to GO TO in call stack.");
    }
    
    public void close_files() throws Exception {                                                     // (285)        CLOSE-FILES.
        end_routine();                                                                               // (286)            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.
        end_rtn_exit();                                                                             
        end_routine_1();                                                                            
        end_routine_12();                                                                           
        end_routine_13();                                                                           
        fileControlService.close(print_file);                                                       
    }
    
    public void terminate_ccvs() throws Exception {                                                  // (287)        TERMINATE-CCVS.
                                                                                                     // (288)       *>     EXIT PROGRAM.
                                                                                                     // (289)       *> TERMINATE-CALL.
        System.exit(0);                                                                              // (290)            STOP     RUN.
    }
    
    public void inspt() throws Exception {                                                           // (291)        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.
        p_or_f.test_results = "INSPT";                                                              
        inspect_counter = inspect_counter.add(BigDecimal.ONE);                                      
    }
    
    public void pass() throws Exception {                                                            // (292)        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.
        p_or_f.test_results = "PASS ";                                                              
        pass_counter = pass_counter.add(BigDecimal.ONE);                                            
    }
    
    public void fail() throws Exception {                                                            // (293)        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.
        p_or_f.test_results = "FAIL*";                                                              
        error_counter = error_counter.add(BigDecimal.ONE);                                          
    }
    
    public void de_lete() throws Exception {                                                         // (294)        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.
        p_or_f.test_results = "*****";                                                              
        delete_counter = delete_counter.add(BigDecimal.ONE);                                        
        re_mark.test_results = "****TEST DELETED****";                                               // (295)            MOVE "****TEST DELETED****" TO RE-MARK.
    }
    
    public void print_detail() throws Exception {                                                    // (296)        PRINT-DETAIL.
        if (rec_ct.compareTo(BigDecimal.ZERO) != 0) {                                                // (297)            IF REC-CT NOT EQUAL TO ZERO
            pardot_x.par_name.test_results = ".";                                                    // (298)                    MOVE "." TO PARDOT-X
            dotvalue.par_name.test_results = rec_ct;                                                 // (299)                    MOVE REC-CT TO DOTVALUE.
        } 
        print_fileContent.print_rec = String.valueOf(test_results);                                  // (300)            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.
        write_line();                                                                               
        if (p_or_f.test_results.compareTo("FAIL*") == 0) {                                           // (301)            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE
            write_line();                                                                           
            fail_routine();                                                                          // (302)               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX
            fail_routine_write();                                                                   
            fail_routine_ex();                                                                      
        } else {                                                                                     // (303)                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.
            bail_out();                                                                             
            bail_out_write();                                                                       
            bail_out_ex();                                                                          
        }
        
        p_or_f.test_results = " ";                                                                   // (304)            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.
        entityService.assignTo(computed_x.test_computed, " ");                                      
        entityService.assignTo(correct_x.test_correct, " ");                                         // (305)            MOVE SPACE TO CORRECT-X.
        if (rec_ct.compareTo(BigDecimal.ZERO) == 0) {                                                // (306)            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.
            entityService.assignTo(par_name.test_results, " ");                                     
        } 
        re_mark.test_results = " ";                                                                  // (307)            MOVE     SPACE TO RE-MARK.
    }
    
    public void head_routine() throws Exception {                                                    // (308)        HEAD-ROUTINE.
        print_fileContent.dummy_record = String.valueOf(ccvs_h_1);                                   // (309)            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        print_fileContent.dummy_record = String.valueOf(ccvs_h_2a);                                  // (310)            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        print_fileContent.dummy_record = String.valueOf(ccvs_h_2b);                                  // (311)            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(3)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        print_fileContent.dummy_record = String.valueOf(ccvs_h_3);                                   // (312)            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(3)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
    }
    
    public void column_names_routine() throws Exception {                                            // (313)        COLUMN-NAMES-ROUTINE.
        print_fileContent.dummy_record = String.valueOf(ccvs_c_1);                                   // (314)            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.
        write_line();                                                                               
        print_fileContent.dummy_record = String.valueOf(ccvs_c_2);                                   // (315)            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        print_fileContent.dummy_record = String.valueOf(hyphen_line);                                // (316)            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.
        write_line();                                                                               
    }
    
    public void end_routine() throws Exception {                                                     // (317)        END-ROUTINE.
        print_fileContent.dummy_record = String.valueOf(hyphen_line);                                // (318)            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(5)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
    }
    
    public void end_rtn_exit() throws Exception {                                                    // (319)        END-RTN-EXIT.
        print_fileContent.dummy_record = String.valueOf(ccvs_e_1);                                   // (320)            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
    }
    
    public void end_routine_1() throws Exception {                                                   // (321)        END-ROUTINE-1.
        error_hold = error_hold.add(error_counter);                                                  // (322)             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO
        error_hold = error_hold.add(inspect_counter);                                               
        error_hold = error_hold.add(delete_counter);                                                 // (323)             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.
        error_hold = error_hold.add(pass_counter);                                                   // (324)             ADD PASS-COUNTER TO ERROR-HOLD.
                                                                                                     // (325)       *>      IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.
        ccvs_e_4_1.ccvs_e_4 = String.valueOf(pass_counter);                                          // (326)             MOVE PASS-COUNTER TO CCVS-E-4-1.
        ccvs_e_4_2.ccvs_e_4 = String.valueOf(error_hold);                                            // (327)             MOVE ERROR-HOLD TO CCVS-E-4-2.
        entityService.assignTo(ccvs_e_2_2.ccvs_e_2, ccvs_e_4);                                       // (328)             MOVE CCVS-E-4 TO CCVS-E-2-2.
        print_fileContent.dummy_record = String.valueOf(ccvs_e_2);                                   // (329)             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.
        write_line();                                                                               
    }
    
    public void end_routine_12() throws Exception {                                                  // (330)         END-ROUTINE-12.
        ender_desc.ccvs_e_2_2.ccvs_e_2 = "TEST(S) FAILED";                                           // (331)             MOVE "TEST(S) FAILED" TO ENDER-DESC.
        if (error_counter.compareTo(BigDecimal.ZERO) == 0) {                                         // (332)            IF       ERROR-COUNTER IS EQUAL TO ZERO
            error_total.ccvs_e_2_2.ccvs_e_2 = "NO ";                                                 // (333)                MOVE "NO " TO ERROR-TOTAL
        } else {                                                                                     // (334)                ELSE
            error_total.ccvs_e_2_2.ccvs_e_2 = String.valueOf(error_counter);                         // (335)                MOVE ERROR-COUNTER TO ERROR-TOTAL.
        }
        
        print_fileContent.dummy_record = String.valueOf(ccvs_e_2);                                   // (336)            MOVE     CCVS-E-2 TO DUMMY-RECORD.
        write_line();                                                                                // (337)            PERFORM WRITE-LINE.
    }
    
    public void end_routine_13() throws Exception {                                                  // (338)        END-ROUTINE-13.
        if (delete_counter.compareTo(BigDecimal.ZERO) == 0) {                                        // (339)            IF DELETE-COUNTER IS EQUAL TO ZERO
            error_total.ccvs_e_2_2.ccvs_e_2 = "NO ";                                                 // (340)                MOVE "NO " TO ERROR-TOTAL  ELSE
        } else {                                                                                    
            error_total.ccvs_e_2_2.ccvs_e_2 = String.valueOf(delete_counter);                        // (341)                MOVE DELETE-COUNTER TO ERROR-TOTAL.
        }
        
        ender_desc.ccvs_e_2_2.ccvs_e_2 = "TEST(S) DELETED     ";                                     // (342)            MOVE "TEST(S) DELETED     " TO ENDER-DESC.
        print_fileContent.dummy_record = String.valueOf(ccvs_e_2);                                   // (343)            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.
        write_line();                                                                               
        if (inspect_counter.compareTo(BigDecimal.ZERO) == 0) {                                       // (344)             IF   INSPECT-COUNTER EQUAL TO ZERO
            error_total.ccvs_e_2_2.ccvs_e_2 = "NO ";                                                 // (345)                 MOVE "NO " TO ERROR-TOTAL
        } else {                                                                                     // (346)             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.
            error_total.ccvs_e_2_2.ccvs_e_2 = String.valueOf(inspect_counter);                      
        }
        
        ender_desc.ccvs_e_2_2.ccvs_e_2 = "TEST(S) REQUIRE INSPECTION";                               // (347)             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.
        print_fileContent.dummy_record = String.valueOf(ccvs_e_2);                                   // (348)             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.
        write_line();                                                                               
        print_fileContent.dummy_record = String.valueOf(ccvs_e_3);                                   // (349)            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.
        write_line();                                                                               
    }
    
    public void write_line() throws Exception {                                                      // (350)        WRITE-LINE.
        record_count = record_count.add(BigDecimal.ONE);                                             // (351)            ADD 1 TO RECORD-COUNT.
                                                                                                     // (352)       *>     IF RECORD-COUNT GREATER 50
                                                                                                     // (353)       *>         MOVE DUMMY-RECORD TO DUMMY-HOLD
                                                                                                     // (354)       *>         MOVE SPACE TO DUMMY-RECORD
                                                                                                     // (355)       *>         WRITE DUMMY-RECORD AFTER ADVANCING PAGE
                                                                                                     // (356)       *>         MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN
                                                                                                     // (357)       *>         MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES
                                                                                                     // (358)       *>         MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN
                                                                                                     // (359)       *>         MOVE DUMMY-HOLD TO DUMMY-RECORD
                                                                                                     // (360)       *>         MOVE ZERO TO RECORD-COUNT.
        wrt_ln();                                                                                    // (361)            PERFORM WRT-LN.
    }
    
    public void wrt_ln() throws Exception {                                                          // (362)        WRT-LN.
        fileControlService.write(print_file);                                                        // (363)            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.
        print_fileContent.dummy_record = " ";                                                        // (364)            MOVE SPACE TO DUMMY-RECORD.
    }
    
    public void blank_line_print() throws Exception {                                                // (365)        BLANK-LINE-PRINT.
        wrt_ln();                                                                                    // (366)            PERFORM WRT-LN.
    }
    
    public void fail_routine() throws Exception {                                                    // (367)        FAIL-ROUTINE.
        if (!entityService.isEmpty(computed_x.test_computed)) {                                      // (368)            IF     COMPUTED-X NOT EQUAL TO SPACE
            fail_routine_write();                                                                    // (369)                   GO TO   FAIL-ROUTINE-WRITE.
            throw new RuntimeException("fail_routine_write must terminate due to GO TO in call stack.");
        } 
        if (!entityService.isEmpty(correct_x.test_correct)) {                                        // (370)            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            fail_routine_write();                                                                   
            throw new RuntimeException("fail_routine_write must terminate due to GO TO in call stack.");
        } 
        inf_ansi_reference.xxinfo = ansi_reference;                                                  // (371)            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.
        entityService.assignTo(info_text.xxinfo, "NO FURTHER INFORMATION, SEE PROGRAM.");            // (372)            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.
        print_fileContent.dummy_record = String.valueOf(xxinfo);                                     // (373)            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        inf_ansi_reference.xxinfo = " ";                                                             // (374)            MOVE   SPACES TO INF-ANSI-REFERENCE.
        fail_routine_ex();                                                                           // (375)            GO TO  FAIL-ROUTINE-EX.
        throw new RuntimeException("fail_routine_ex must terminate due to GO TO in call stack.");
    }
    
    public void fail_routine_write() throws Exception {                                              // (376)        FAIL-ROUTINE-WRITE.
        print_fileContent.print_rec = String.valueOf(test_computed);                                 // (377)            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE
        write_line();                                                                               
        cor_ansi_reference.correct_x.test_correct = ansi_reference;                                  // (378)            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.
        print_fileContent.print_rec = String.valueOf(test_correct);                                  // (379)            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        cor_ansi_reference.correct_x.test_correct = " ";                                             // (380)            MOVE   SPACES TO COR-ANSI-REFERENCE.
    }
    
    public void fail_routine_ex() throws Exception {                                                 // (381)        FAIL-ROUTINE-EX. EXIT.
        System.exit(0);                                                                             
    }
    
    public void bail_out() throws Exception {                                                        // (382)        BAIL-OUT.
        if (!Strings.isNullOrEmpty(computed_a.computed_x.test_computed)) {                           // (383)            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.
            bail_out_write();                                                                       
            throw new RuntimeException("bail_out_write must terminate due to GO TO in call stack.");
        } 
        if (Strings.isNullOrEmpty(correct_a.correct_x.test_correct)) {                               // (384)            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.
            bail_out_ex();                                                                          
            throw new RuntimeException("bail_out_ex must terminate due to GO TO in call stack.");
        } 
    }
    
    public void bail_out_write() throws Exception {                                                  // (385)        BAIL-OUT-WRITE.
        xxcorrect.info_text.xxinfo = correct_a.correct_x.test_correct;                               // (386)            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.
        xxcomputed.info_text.xxinfo = computed_a.computed_x.test_computed;                          
        inf_ansi_reference.xxinfo = ansi_reference;                                                  // (387)            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.
        print_fileContent.dummy_record = String.valueOf(xxinfo);                                     // (388)            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            write_line();                                                                           
        }                                                                                           
                                                                                                    
        inf_ansi_reference.xxinfo = " ";                                                             // (389)            MOVE   SPACES TO INF-ANSI-REFERENCE.
    }
    
    public void bail_out_ex() throws Exception {                                                     // (390)        BAIL-OUT-EX. EXIT.
        System.exit(0);                                                                             
    }
    
    public void ccvs1_exit() throws Exception {                                                      // (391)        CCVS1-EXIT.
        System.exit(0);                                                                              // (392)            EXIT.
    }
    
    public void sect_rl101_001() throws Exception {                                                  // (393)        SECT-RL101-001 SECTION.
        rel_init_001();
        rel_test_001();
        rel_delete_001();
        rel_fail_001();
        rel_write_001();
        rel_init_002();
        rel_test_002();
        rel_delete_002();
        rel_test_002_1();
        rel_write_002();
    }
    
    public void rel_init_001() throws Exception {                                                    // (394)        REL-INIT-001.
        feature.test_results = "FILE CREATE RL-FS2";                                                 // (395)            MOVE     "FILE CREATE RL-FS2" TO FEATURE.
        fileControlService.openOutput(rl_fs2);                                                       // (396)            OPEN     OUTPUT    RL-FS2.
        xfile_name.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = "RL-FS2"; // (397)            MOVE     "RL-FS2" TO XFILE-NAME (1).
        xrecord_name.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = "R1-F-G"; // (398)            MOVE     "R1-F-G" TO XRECORD-NAME (1).
        xprogram_name.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = ccvs_pgm_id; // (399)            MOVE CCVS-PGM-ID  TO XPROGRAM-NAME (1).
        xrecord_length.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = BigDecimal.valueOf(120); // (400)            MOVE     000120   TO XRECORD-LENGTH (1).
        chars_or_records.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = "RC"; // (401)            MOVE     "RC"     TO CHARS-OR-RECORDS (1).
        xblock_size.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = BigDecimal.ONE; // (402)            MOVE     0001     TO XBLOCK-SIZE (1).
        records_in_file.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = BigDecimal.valueOf(500); // (403)            MOVE     000500   TO RECORDS-IN-FILE (1).
        xfile_organization.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = "RL"; // (404)            MOVE     "RL"     TO XFILE-ORGANIZATION (1).
        xlabel_type.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = "S"; // (405)            MOVE     "S"      TO XLABEL-TYPE (1).
        xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = BigDecimal.ONE; // (406)            MOVE     000001   TO XRECORD-NUMBER (1).
    }
    
    public void rel_test_001() throws Exception {                                                    // (407)        REL-TEST-001.
        entityService.assignTo(rl_fs2Content.rl_fs2r1_f_g_120, file_record_info_p1_120.file_record_info.get(0).file_record_information_rec); // (408)            MOVE     FILE-RECORD-INFO-P1-120 (1) TO RL-FS2R1-F-G-120.
        fileControlService.write(rl_fs2);                                                            // (409)            WRITE    RL-FS2R1-F-G-120
                                                                                                     // (410)                     INVALID KEY GO TO REL-FAIL-001.
        if (xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec.compareTo(BigDecimal.valueOf(500)) == 0) { // (411)            IF      XRECORD-NUMBER (1) EQUAL TO 500
            rel_write_001();                                                                         // (412)                    GO TO REL-WRITE-001.
            throw new RuntimeException("rel_write_001 must terminate due to GO TO in call stack.");
        } 
        xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec = xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec.add(BigDecimal.ONE); // (413)            ADD      000001 TO XRECORD-NUMBER (1).
        rel_test_001();                                                                              // (414)            GO       TO REL-TEST-001.
    }
    
    public void rel_delete_001() throws Exception {                                                  // (415)        REL-DELETE-001.
        de_lete();                                                                                   // (416)            PERFORM   DE-LETE.
        rel_write_001();                                                                             // (417)            GO TO REL-WRITE-001.
        throw new RuntimeException("rel_write_001 must terminate due to GO TO in call stack.");
    }
    
    public void rel_fail_001() throws Exception {                                                    // (418)        REL-FAIL-001.
        fail();                                                                                      // (419)            PERFORM   FAIL.
        re_mark.test_results = "BOUNDARY VIOLATION";                                                 // (420)            MOVE    "BOUNDARY VIOLATION"  TO RE-MARK.
    }
    
    public void rel_write_001() throws Exception {                                                   // (421)        REL-WRITE-001.
        entityService.assignTo(par_name.test_results, "REL-TEST-001");                               // (422)            MOVE     "REL-TEST-001" TO   PAR-NAME
        computed_a.computed_x.test_computed = "FILE CREATED, LFILE ";                                // (423)            MOVE     "FILE CREATED, LFILE "  TO COMPUTED-A.
        correct_18v0.cr_18v0.correct_x.test_correct = xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec; // (424)            MOVE    XRECORD-NUMBER (1) TO CORRECT-18V0.
        print_detail();                                                                              // (425)            PERFORM  PRINT-DETAIL.
        fileControlService.close(rl_fs2);                                                            // (426)            CLOSE    RL-FS2.
    }
    
    public void rel_init_002() throws Exception {                                                    // (427)        REL-INIT-002.
        fileControlService.openInput(rl_fs2);                                                        // (428)            OPEN     INPUT     RL-FS2.
        wrk_cs_09v00 = BigDecimal.ZERO;                                                              // (429)            MOVE     ZERO      TO WRK-CS-09V00.
    }
    
    public void rel_test_002() throws Exception {                                                    // (430)        REL-TEST-002.
        fileControlService.read(rl_fs2);                                                             // (431)            READ     RL-FS2
                                                                                                     // (432)                     AT END GO TO REL-TEST-002-1.
        entityService.assignTo(file_record_info_p1_120.file_record_info.get(0).file_record_information_rec, rl_fs2Content.rl_fs2r1_f_g_120); // (433)            MOVE     RL-FS2R1-F-G-120 TO FILE-RECORD-INFO-P1-120 (1).
        wrk_cs_09v00 = wrk_cs_09v00.add(BigDecimal.ONE);                                             // (434)            ADD      1 TO WRK-CS-09V00.
        if (wrk_cs_09v00.compareTo(BigDecimal.valueOf(500)) > 0) {                                   // (435)            IF       WRK-CS-09V00 GREATER 500
            re_mark.test_results = "MORE THAN 500 RECORDS";                                          // (436)                    MOVE "MORE THAN 500 RECORDS" TO RE-MARK
            rel_test_002_1();                                                                        // (437)                     GO TO REL-TEST-002-1.
            throw new RuntimeException("rel_test_002_1 must terminate due to GO TO in call stack.");
        } 
        rel_test_002();                                                                              // (438)            GO       TO REL-TEST-002.
    }
    
    public void rel_delete_002() throws Exception {                                                  // (439)        REL-DELETE-002.
    }
    
    public void rel_test_002_1() throws Exception {                                                  // (440)        REL-TEST-002-1.
        if (xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec.compareTo(BigDecimal.valueOf(500)) != 0) { // (441)            IF       XRECORD-NUMBER (1) NOT EQUAL TO 500
            fail();                                                                                  // (442)                     PERFORM FAIL
        } else {                                                                                     // (443)                     ELSE
            pass();                                                                                  // (444)                     PERFORM PASS.
        }
        
        rel_write_002();                                                                             // (445)            GO       TO REL-WRITE-002.
        throw new RuntimeException("rel_write_002 must terminate due to GO TO in call stack.");
    }
    
    public void rel_write_002() throws Exception {                                                   // (446)        REL-WRITE-002.
        entityService.assignTo(par_name.test_results, "REL-TEST-002");                               // (447)            MOVE     "REL-TEST-002" TO PAR-NAME.
        computed_a.computed_x.test_computed = "FILE VERIFIED, LFILE";                                // (448)            MOVE     "FILE VERIFIED, LFILE" TO COMPUTED-A.
        correct_18v0.cr_18v0.correct_x.test_correct = xrecord_number.file_record_info_p1_120.file_record_info.get(0).file_record_information_rec; // (449)            MOVE    XRECORD-NUMBER (1) TO CORRECT-18V0.
        print_detail();                                                                              // (450)            PERFORM  PRINT-DETAIL.
        fileControlService.close(rl_fs2);                                                            // (451)            CLOSE   RL-FS2.
    }
    
    public void ccvs_exit() throws Exception {                                                       // (452)        CCVS-EXIT SECTION.
        ccvs_999999();
    }
    
    public void ccvs_999999() throws Exception {                                                     // (453)        CCVS-999999.
        close_files();                                                                               // (454)            GO TO CLOSE-FILES.
        throw new RuntimeException("close_files must terminate due to GO TO in call stack.");
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RL101A.class);
        final RL101A rl101a = context.getBean(RL101A.class);
        rl101a.ccvs1();
    }
}
