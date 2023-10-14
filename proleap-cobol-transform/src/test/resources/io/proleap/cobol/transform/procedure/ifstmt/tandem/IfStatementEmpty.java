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
public class IfStatementEmpty {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCR01                                                                        //   (2)  PROGRAM-ID. DATADESCR01.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class ITEMSType {                                                                         //   (5)  01 ITEMS.
        @Size(max=10)                                                                                //   (6)     02 ITEM1 PIC X(10).
        protected String item1;                                                                     
        @Size(max=10)                                                                                //   (7)     02 ITEM2 PIC X(10).
        protected String item2;                                                                     
    }
    
    protected ITEMSType items = new ITEMSType();
    public void procedureDivision() throws Exception{                                                //   (8)  PROCEDURE DIVISION.
        if (entityService.isEmpty(items)) {                                                          //   (9)     IF ITEMS EQUAL TO SPACE
            System.out.println("empty");                                                             //  (10)        DISPLAY 'empty'.
        } 
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, IfStatementEmpty.class);
        final IfStatementEmpty ifstatementempty = context.getBean(IfStatementEmpty.class);
        ifstatementempty.procedureDivision();
    }
}
