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
public class IfStatementBooleanAnd {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCR01                                                                        //   (2)  PROGRAM-ID. DATADESCR01.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public void procedureDivision() throws Exception{                                                //   (5)  PROCEDURE DIVISION.
        if (item1&&item2) {                                                                          //   (6)     IF ITEM1 AND ITEM2
            System.out.println("empty");                                                             //   (7)        DISPLAY 'empty'.
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, IfStatementBooleanAnd.class);
        final IfStatementBooleanAnd ifstatementbooleanand = context.getBean(IfStatementBooleanAnd.class);
        ifstatementbooleanand.procedureDivision();
    }
}
