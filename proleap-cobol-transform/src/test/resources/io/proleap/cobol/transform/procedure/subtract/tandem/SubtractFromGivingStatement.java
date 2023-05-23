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
public class SubtractFromGivingStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: SUBTRSTMT                                                                          //   (2)  PROGRAM-ID. SUBTRSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=10, fraction=0)                                                                  //   (5)  77 SOMEID1 PIC 9(10).
    protected BigDecimal someid1;                                                                   
    @Digits(integer=10, fraction=0)                                                                  //   (6)  77 SOMEID2 PIC 9(10).
    protected BigDecimal someid2;                                                                   
    @Digits(integer=10, fraction=0)                                                                  //   (7)  77 SOMEID3 PIC 9(10).
    protected BigDecimal someid3;                                                                   
    @Digits(integer=10, fraction=0)                                                                  //   (8)  77 SOMEID4 PIC 9(10).
    protected BigDecimal someid4;                                                                   
    public void procedureDivision() throws Exception{                                                //   (9)  PROCEDURE DIVISION.
        someid3 = someid2.subtract(someid1).subtract(BigDecimal.ONE);                                //  (10)      SUBTRACT SOMEID1 1 FROM SOMEID2 GIVING SOMEID3 SOMEID4 ROUNDED.
        someid4 = someid2.subtract(someid1).subtract(BigDecimal.ONE);                               
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, SubtractFromGivingStatement.class);
        final SubtractFromGivingStatement subtractfromgivingstatement = context.getBean(SubtractFromGivingStatement.class);
        subtractfromgivingstatement.procedureDivision();
    }
}
