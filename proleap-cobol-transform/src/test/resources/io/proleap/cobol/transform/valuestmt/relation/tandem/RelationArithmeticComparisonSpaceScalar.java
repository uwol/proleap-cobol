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
public class RelationArithmeticComparisonSpaceScalar {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: RELEMPTY                                                                           //   (2)  PROGRAM-ID. RELEMPTY.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=20)                                                                                    //   (5)  01 SOMETEXT PIC X(20).
    protected String sometext;                                                                      
    public void procedureDivision() throws Exception{                                                //   (6)  PROCEDURE DIVISION.
        if (Strings.isNullOrEmpty(sometext)) {                                                       //   (7)     IF SOMETEXT = SPACE
            System.out.println("test");                                                              //   (8)        DISPLAY 'test'
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RelationArithmeticComparisonSpaceScalar.class);
        final RelationArithmeticComparisonSpaceScalar relationarithmeticcomparisonspacescalar = context.getBean(RelationArithmeticComparisonSpaceScalar.class);
        relationarithmeticcomparisonspacescalar.procedureDivision();
    }
}
