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
public class MoveToStatementFile {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: MOVETOSTMT                                                                         //   (2)  PROGRAM-ID. MOVETOSTMT.
    */
                                                                                                     //   (3)  ENVIRONMENT DIVISION.
                                                                                                     //   (4)     INPUT-OUTPUT SECTION.
    @Inject                                                                                          //   (5)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry rl_fs2 = new FileControlEntry();                                                //   (6)           SELECT RL-FS2.
    @Inject                                                                                          //   (7)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (8)     FILE SECTION.
    public class Rl_fs2Type {                                                                        //   (9)        FD RL-FS2.
        public class Somefile1_100Type {                                                             //  (10)           01 SOMEFILE1-100.
            @Size(max=100)                                                                           //  (11)              02 FILLER PIC X(100).
            protected String filler0;                                                               
        }
        
        protected Somefile1_100Type somefile1_100 = new Somefile1_100Type();
    }
    public Rl_fs2Type rl_fs2Content = new Rl_fs2Type();
                                                                                                     //  (12)     WORKING-STORAGE SECTION.
    public class File_record_information_recType {                                                   //  (13)        01 FILE-RECORD-INFORMATION-REC.
        public class File_record_infoType {                                                          //  (14)           03 FILE-RECORD-INFO OCCURS 10 TIMES.
            public class File_record_info_p1_100Type {                                               //  (15)              05 FILE-RECORD-INFO-P1-100.
                @Size(max=10)                                                                        //  (16)                 07 XFILE-NAME PIC X(10).
                protected String xfile_name;                                                        
                @Size(max=90)                                                                        //  (17)                 07 FILLER PIC X(90).
                protected String filler1;                                                           
            }
            
            protected File_record_info_p1_100Type file_record_info_p1_100 = new File_record_info_p1_100Type();
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
    public void procedureDivision() throws Exception{                                                //  (18)  PROCEDURE DIVISION.
        entityService.assignTo(file_record_info_p1_100.file_record_info.get(0).file_record_information_rec, rl_fs2Content.somefile1_100); //  (19)      MOVE SOMEFILE1-100 TO FILE-RECORD-INFO-P1-100 (1).
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, MoveToStatementFile.class);
        final MoveToStatementFile movetostatementfile = context.getBean(MoveToStatementFile.class);
        movetostatementfile.procedureDivision();
    }
}
