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
public class MoveToStatementGroup {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: MOVETOSTMT                                                                         //   (2)  PROGRAM-ID. MOVETOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class ITEMS1Type {                                                                        //   (5)  01 ITEMS1.
        @Size(max=10)                                                                                //   (6)     02 ITEM1 PIC X(10).
        protected String item1;                                                                     
        @Size(max=10)                                                                                //   (7)     02 ITEM2 PIC X(10).
        protected String item2;                                                                     
    }
    
    protected ITEMS1Type items1 = new ITEMS1Type();
    public class ITEMS2Type {                                                                        //   (8)  01 ITEMS2.
        @Size(max=10)                                                                                //   (9)     02 ITEM1 PIC X(10).
        protected String item1;                                                                     
        @Size(max=10)                                                                                //  (10)     02 ITEM2 PIC X(10).
        protected String item2;                                                                     
    }
    
    protected ITEMS2Type items2 = new ITEMS2Type();
    public void procedureDivision() throws Exception{                                                //  (11)  PROCEDURE DIVISION.
        entityService.assignTo(items2, items1);                                                      //  (12)      MOVE ITEMS1 TO ITEMS2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, MoveToStatementGroup.class);
        final MoveToStatementGroup movetostatementgroup = context.getBean(MoveToStatementGroup.class);
        movetostatementgroup.procedureDivision();
    }
}
