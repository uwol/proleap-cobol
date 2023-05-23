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
public class Length {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: LGTH                                                                               //   (2)  PROGRAM-ID. LGTH.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class SOMEGROUPType {                                                                     //   (5)  01 SOMEGROUP.
        @Size(max=20)                                                                                //   (6)     05 SOMETEXT PIC X(20).
        protected String sometext;                                                                  
    }
    
    protected SOMEGROUPType somegroup = new SOMEGROUPType();
    @Digits(integer=10, fraction=0)                                                                  //   (7)  01 SOMELENGTH PIC 9(10).
    protected BigDecimal somelength;                                                                
    public void procedureDivision() throws Exception{                                                //   (8)  PROCEDURE DIVISION.
        somelength = entityService.getLength(somegroup);                                             //   (9)     MOVE LENGTH OF SOMEGROUP TO SOMELENGTH.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, Length.class);
        final Length length = context.getBean(Length.class);
        length.procedureDivision();
    }
}
