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
public class PerformProcedureTimes {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMPROCEDURETIMES                                                              //   (2)  PROGRAM-ID. PERFORMPROCEDURETIMES.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){
            proc1();                                                                                 //   (4)    PERFORM PROC1
        }                                                                                            //   (5)      2 TIMES.
                                                                                                    
    }
    public void proc1() throws Exception {                                                           //   (6)    PROC1.
        System.out.println("test");                                                                  //   (7)      DISPLAY 'test'.
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformProcedureTimes.class);
        final PerformProcedureTimes performproceduretimes = context.getBean(PerformProcedureTimes.class);
        performproceduretimes.procedureDivision();
    }
}
