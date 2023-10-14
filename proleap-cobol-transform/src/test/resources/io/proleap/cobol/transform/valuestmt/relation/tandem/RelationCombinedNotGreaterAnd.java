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
public class RelationCombinedNotGreaterAnd {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: COND                                                                               //   (2)  PROGRAM-ID. COND.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        if (true) {                                                                                  //   (4)     IF -4 IS NOT GREATER (2 AND 3 AND ZERO AND 1 - 5) END-IF.
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RelationCombinedNotGreaterAnd.class);
        final RelationCombinedNotGreaterAnd relationcombinednotgreaterand = context.getBean(RelationCombinedNotGreaterAnd.class);
        relationcombinednotgreaterand.procedureDivision();
    }
}
