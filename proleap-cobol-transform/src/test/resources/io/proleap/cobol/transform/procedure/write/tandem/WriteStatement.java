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
public class WriteStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: TERMINATESTMT                                                                      //   (2)  PROGRAM-ID. TERMINATESTMT.
    */
                                                                                                     //   (3)  ENVIRONMENT DIVISION.
                                                                                                     //   (4)     INPUT-OUTPUT SECTION.
    @Inject                                                                                          //   (5)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry somefile1 = new FileControlEntry("somefile1.txt");                              //   (6)           SELECT SOMEFILE1 ASSIGN TO 'somefile1.txt'.
    @Inject                                                                                          //   (7)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (8)     FILE SECTION.
    public class SOMEFILE1Type {                                                                     //   (9)        FD SOMEFILE1.
        protected Object items;                                                                      //  (10)           01 ITEMS.
    }
    public SOMEFILE1Type somefile1Content = new SOMEFILE1Type();
    public void procedureDivision() throws Exception{                                                //  (11)  PROCEDURE DIVISION.
        fileControlService.write(somefile1);                                                         //  (12)     WRITE ITEMS
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, WriteStatement.class);
        final WriteStatement writestatement = context.getBean(WriteStatement.class);
        writestatement.procedureDivision();
    }
}
