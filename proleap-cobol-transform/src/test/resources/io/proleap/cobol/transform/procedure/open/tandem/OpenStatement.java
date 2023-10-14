import java.lang.String;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import jakarta.validation.constraints.*;
import io.proleap.cobol.api.*;
import io.proleap.cobol.api.data.*;
import io.proleap.cobol.api.environment.configuration.object.*;
import io.proleap.cobol.api.environment.configuration.source.*;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.*;
import io.proleap.cobol.api.ProLeapCobolApiSpringConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

@Singleton
public class OpenStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: OPENSTMT                                                                           //   (2)  PROGRAM-ID. OPENSTMT.
    */
                                                                                                     //   (3)  ENVIRONMENT DIVISION.
                                                                                                     //   (4)     INPUT-OUTPUT SECTION.
    @Inject                                                                                          //   (5)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry somefile1 = new FileControlEntry("somefile1.txt");                              //   (6)           SELECT SOMEFILE1 ASSIGN TO 'somefile1.txt'.
    FileControlEntry somefile2 = new FileControlEntry("somefile2.txt");                              //   (7)           SELECT SOMEFILE2 ASSIGN TO 'somefile2.txt'.
    FileControlEntry somefile3 = new FileControlEntry("somefile3.txt");                              //   (8)           SELECT SOMEFILE3 ASSIGN TO 'somefile3.txt'.
    FileControlEntry somefile4 = new FileControlEntry("somefile4.txt");                              //   (9)           SELECT SOMEFILE4 ASSIGN TO 'somefile4.txt'.
    FileControlEntry somefile5 = new FileControlEntry("somefile5.txt");                              //  (10)           SELECT SOMEFILE5 ASSIGN TO 'somefile5.txt'.
    @Inject                                                                                          //  (11)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //  (12)     FILE SECTION.
    public class SOMEFILE1Type {                                                                     //  (13)          FD SOMEFILE1.
    }
    public SOMEFILE1Type somefile1Content = new SOMEFILE1Type();
    public class SOMEFILE2Type {                                                                     //  (14)          FD SOMEFILE2.
    }
    public SOMEFILE2Type somefile2Content = new SOMEFILE2Type();
    public class SOMEFILE3Type {                                                                     //  (15)          FD SOMEFILE3.
    }
    public SOMEFILE3Type somefile3Content = new SOMEFILE3Type();
    public class SOMEFILE4Type {                                                                     //  (16)          FD SOMEFILE4.
    }
    public SOMEFILE4Type somefile4Content = new SOMEFILE4Type();
    public class SOMEFILE5Type {                                                                     //  (17)          FD SOMEFILE5.
    }
    public SOMEFILE5Type somefile5Content = new SOMEFILE5Type();
    public void procedureDivision() throws Exception{                                                //  (18)  PROCEDURE DIVISION.
                                                                                                     //  (19)     OPEN
                                                                                                     //  (20)        INPUT
        fileControlService.openInput(somefile1);                                                     //  (21)           SOMEFILE1 WITH NO REWIND
        fileControlService.openInput(somefile2);                                                     //  (22)           SOMEFILE2 REVERSED
                                                                                                     //  (23)        OUTPUT
                                                                                                     //  (24)           SOMEFILE3 WITH NO REWIND
                                                                                                     //  (25)        I-O
        fileControlService.openInputOutput(somefile4);                                               //  (26)           SOMEFILE4 SOMEFILE5
        fileControlService.openInputOutput(somefile5);                                              
        fileControlService.openOutput(somefile3);                                                   
                                                                                                     //  (27)        EXTEND
        fileControlService.openExtend(somefile4);                                                    //  (28)           SOMEFILE4 SOMEFILE5.
        fileControlService.openExtend(somefile5);                                                   
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, OpenStatement.class);
        final OpenStatement openstatement = context.getBean(OpenStatement.class);
        openstatement.procedureDivision();
    }
}
