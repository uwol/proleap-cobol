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
public class RelationArithmeticComparisonSpaceGroup {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: RELEMPTY                                                                           //   (2)  PROGRAM-ID. RELEMPTY.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class SOMEGROUPType {                                                                     //   (5)  01 SOMEGROUP.
        @Size(max=20)                                                                                //   (6)     05 SOMETEXT PIC X(20).
        protected String sometext;                                                                  
    }
    
    protected SOMEGROUPType somegroup = new SOMEGROUPType();
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        if (entityService.isEmpty(somegroup)) {                                                      //   (8)     IF SOMEGROUP = SPACE
            System.out.println("test");                                                              //   (9)        DISPLAY 'test'
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, RelationArithmeticComparisonSpaceGroup.class);
        final RelationArithmeticComparisonSpaceGroup relationarithmeticcomparisonspacegroup = context.getBean(RelationArithmeticComparisonSpaceGroup.class);
        relationarithmeticcomparisonspacegroup.procedureDivision();
    }
}
