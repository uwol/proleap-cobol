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
public class PerformInlineTimes {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMINLINETIMES                                                                 //   (2)  PROGRAM-ID. PERFORMINLINETIMES.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        
                                                                                                     //   (4)    PERFORM
        for(BigDecimal i=BigDecimal.ZERO; i.compareTo(BigDecimal.valueOf(2)) < 0; i = i.add(BigDecimal.ONE)){ //   (5)      2 TIMES
            System.out.println("Test");                                                              //   (6)        DISPLAY "Test"
        }
        
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformInlineTimes.class);
        final PerformInlineTimes performinlinetimes = context.getBean(PerformInlineTimes.class);
        performinlinetimes.procedureDivision();
    }
}
