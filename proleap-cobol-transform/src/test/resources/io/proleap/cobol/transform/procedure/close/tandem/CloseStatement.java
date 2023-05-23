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
public class CloseStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: CLSSTMT                                                                            //   (2)  PROGRAM-ID. CLSSTMT.
    */
                                                                                                     //   (3)  ENVIRONMENT DIVISION.
                                                                                                     //   (4)     INPUT-OUTPUT SECTION.
    @Inject                                                                                          //   (5)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry somefile1a = new FileControlEntry("somefile1a.txt");                            //   (6)           SELECT SOMEFILE1A ASSIGN TO 'somefile1a.txt'.
    FileControlEntry somefile1b = new FileControlEntry("somefile1b.txt");                            //   (7)           SELECT SOMEFILE1B ASSIGN TO 'somefile1b.txt'.
    FileControlEntry somefile2 = new FileControlEntry("somefile2.txt");                              //   (8)           SELECT SOMEFILE2 ASSIGN TO 'somefile2.txt'.
    FileControlEntry somefile3 = new FileControlEntry("somefile3.txt");                              //   (9)           SELECT SOMEFILE3 ASSIGN TO 'somefile3.txt'.
    FileControlEntry somefile4 = new FileControlEntry("somefile4.txt");                              //  (10)           SELECT SOMEFILE4 ASSIGN TO 'somefile4.txt'.
    FileControlEntry somefile5 = new FileControlEntry("somefile5.txt");                              //  (11)           SELECT SOMEFILE5 ASSIGN TO 'somefile5.txt'.
    @Inject                                                                                          //  (12)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //  (13)     FILE SECTION.
    public class SOMEFILE1AType {                                                                    //  (14)          FD SOMEFILE1A.
    }
    public SOMEFILE1AType somefile1aContent = new SOMEFILE1AType();
    public class SOMEFILE1BType {                                                                    //  (15)          FD SOMEFILE1B.
    }
    public SOMEFILE1BType somefile1bContent = new SOMEFILE1BType();
    public class SOMEFILE2Type {                                                                     //  (16)          FD SOMEFILE2.
    }
    public SOMEFILE2Type somefile2Content = new SOMEFILE2Type();
    public class SOMEFILE3Type {                                                                     //  (17)          FD SOMEFILE3.
    }
    public SOMEFILE3Type somefile3Content = new SOMEFILE3Type();
    public class SOMEFILE4Type {                                                                     //  (18)          FD SOMEFILE4.
    }
    public SOMEFILE4Type somefile4Content = new SOMEFILE4Type();
    public class SOMEFILE5Type {                                                                     //  (19)          FD SOMEFILE5.
    }
    public SOMEFILE5Type somefile5Content = new SOMEFILE5Type();
    public void procedureDivision() throws Exception{                                                //  (20)  PROCEDURE DIVISION.
                                                                                                     //  (21)     CLOSE
        fileControlService.close(somefile1a);                                                        //  (22)        SOMEFILE1A UNIT FOR REMOVAL WITH LOCK
        fileControlService.close(somefile1b);                                                        //  (23)        SOMEFILE1B REEL.
                                                                                                     //  (24)     CLOSE
        fileControlService.close(somefile2);                                                         //  (25)        SOMEFILE2 WITH LOCK.
                                                                                                     //  (26)     CLOSE
        fileControlService.close(somefile3);                                                         //  (27)        SOMEFILE3 WITH WAIT USING CLOSE-DISPOSITION ORDERLY.
                                                                                                     //  (28)     CLOSE
        fileControlService.close(somefile4);                                                         //  (29)        SOMEFILE4 WITH NO WAIT USING ASSOCIATED-DATA 4.
                                                                                                     //  (30)     CLOSE
        fileControlService.close(somefile5);                                                         //  (31)        SOMEFILE5 WITH NO WAIT USING ASSOCIATED-DATA-LENGTH SOMEID1.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, CloseStatement.class);
        final CloseStatement closestatement = context.getBean(CloseStatement.class);
        closestatement.procedureDivision();
    }
}
