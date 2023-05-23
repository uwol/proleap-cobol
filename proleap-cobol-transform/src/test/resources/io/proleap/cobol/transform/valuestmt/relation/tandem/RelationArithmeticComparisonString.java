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
public class RelationArithmeticComparisonString {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: RELSTRING                                                                          //   (2)  PROGRAM-ID. RELSTRING.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=5)                                                                                     //   (5)  02 P-OR-F PIC X(5) VALUE SPACE.
    protected String p_or_f = " ";                                                                  
    public void procedureDivision() throws Exception{                                                //   (6)  PROCEDURE DIVISION.
        if (p_or_f.compareTo("FAIL*") == 0) {                                                        //   (7)     IF P-OR-F EQUAL TO "FAIL*"
            System.out.println("test");                                                              //   (8)        DISPLAY "test".
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RelationArithmeticComparisonString.class);
        final RelationArithmeticComparisonString relationarithmeticcomparisonstring = context.getBean(RelationArithmeticComparisonString.class);
        relationarithmeticcomparisonstring.procedureDivision();
    }
}
