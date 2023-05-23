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
public class PerformInlineVarying {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PERFORMINLINEVARYING                                                               //   (2)  PROGRAM-ID. PERFORMINLINEVARYING.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
                                                                                                     //   (4)    PERFORM VARYING SOMEID FROM 1 BY 2 UNTIL SOMEID = 9
        System.out.println("Test");                                                                  //   (5)        DISPLAY "Test"
        System.exit(0);                                                                              //   (6)        STOP RUN
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, PerformInlineVarying.class);
        final PerformInlineVarying performinlinevarying = context.getBean(PerformInlineVarying.class);
        performinlinevarying.procedureDivision();
    }
}
