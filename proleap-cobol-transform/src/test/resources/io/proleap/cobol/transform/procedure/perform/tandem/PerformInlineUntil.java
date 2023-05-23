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
public class PerformInlineUntil {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMINLINEUNTIL                                                                 //   (2)  PROGRAM-ID. PERFORMINLINEUNTIL.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        
        do {
                                                                                                     //   (4)    PERFORM
                                                                                                     //   (5)      WITH TEST AFTER
                                                                                                     //   (6)      UNTIL 1=1
            System.out.println("Test");                                                              //   (7)        DISPLAY "Test"
            System.exit(0);                                                                          //   (8)        STOP RUN
        } while(!(true));                                                                           
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformInlineUntil.class);
        final PerformInlineUntil performinlineuntil = context.getBean(PerformInlineUntil.class);
        performinlineuntil.procedureDivision();
    }
}
