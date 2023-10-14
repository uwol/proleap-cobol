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
public class ReadStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: READSTMT                                                                           //   (2)  PROGRAM-ID. READSTMT.
    */
                                                                                                     //   (3)  ENVIRONMENT DIVISION.
                                                                                                     //   (4)     INPUT-OUTPUT SECTION.
    @Inject                                                                                          //   (5)        FILE-CONTROL.
    FileControlService fileControlService;
    
    FileControlEntry somefile1 = new FileControlEntry("somefile1.txt");                              //   (6)           SELECT SOMEFILE1 ASSIGN TO 'somefile1.txt'.
    @Inject                                                                                          //   (7)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (8)     WORKING-STORAGE SECTION.
    protected Object items;                                                                          //   (9)        01 ITEMS.
    public void procedureDivision() throws Exception{                                                //  (10)  PROCEDURE DIVISION.
        fileControlService.read(somefile1, items);                                                   //  (11)     READ SOMEFILE1
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ReadStatement.class);
        final ReadStatement readstatement = context.getBean(ReadStatement.class);
        readstatement.procedureDivision();
    }
}
