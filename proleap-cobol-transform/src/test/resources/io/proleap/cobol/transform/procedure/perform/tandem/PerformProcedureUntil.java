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
public class PerformProcedureUntil {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMPROCEDURETIMES                                                              //   (2)  PROGRAM-ID. PERFORMPROCEDURETIMES.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        do {
            proc1();                                                                                 //   (4)    PERFORM PROC1
        } while(!(true));                                                                            //   (5)      WITH TEST
                                                                                                    
    }
                                                                                                     //   (6)      AFTER UNTIL 1=1.
    public void proc1() throws Exception {                                                           //   (7)    PROC1.
        System.out.println("test");                                                                  //   (8)      DISPLAY 'test'.
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformProcedureUntil.class);
        final PerformProcedureUntil performprocedureuntil = context.getBean(PerformProcedureUntil.class);
        performprocedureuntil.procedureDivision();
    }
}
