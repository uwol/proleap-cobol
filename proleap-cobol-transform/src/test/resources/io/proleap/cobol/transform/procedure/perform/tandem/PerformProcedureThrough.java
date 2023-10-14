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
public class PerformProcedureThrough {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMPROCEDURETHROUGH                                                            //   (2)  PROGRAM-ID. PERFORMPROCEDURETHROUGH.
    */
                                                                                                     //   (3)  PROCEDURE DIVISION.
    public void init() throws Exception {                                                            //   (4)  INIT.
        proc2();                                                                                     //   (5)      PERFORM PROC2 THROUGH PROC4.
        proc3();                                                                                    
        proc4();                                                                                    
        proc3();                                                                                     //   (6)      PERFORM PROC3.
        System.exit(0);                                                                              //   (7)      STOP RUN.
    }
    
    public void proc1() throws Exception {                                                           //   (8)  PROC1.
        System.out.println("Proc1");                                                                 //   (9)      Display "Proc1".
    }
    
    public void proc2() throws Exception {                                                           //  (10)  PROC2.
        System.out.println("Proc2");                                                                 //  (11)      Display "Proc2".
    }
    
    public void proc3() throws Exception {                                                           //  (12)  PROC3.
        System.out.println("Proc3");                                                                 //  (13)      Display "Proc3".
    }
    
    public void proc4() throws Exception {                                                           //  (14)  PROC4.
        System.out.println("Proc4");                                                                 //  (15)      Display "Proc4".
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformProcedureThrough.class);
        final PerformProcedureThrough performprocedurethrough = context.getBean(PerformProcedureThrough.class);
        performprocedurethrough.init();
    }
}
