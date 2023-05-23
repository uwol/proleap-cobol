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
public class ArithmeticExpressionMultiplyAdd {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ARITHEXPR                                                                          //   (2)  PROGRAM-ID. ARITHEXPR.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=20, fraction=0)                                                                  //   (5)     01 SOMEID1 PIC 9(20).
    protected BigDecimal someid1;                                                                   
    public void procedureDivision() throws Exception{                                                //   (6)  PROCEDURE DIVISION.
        someid1 = BigDecimal.valueOf(17);                                                            //   (7)     COMPUTE SOMEID1 = 5 * 3 + 2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ArithmeticExpressionMultiplyAdd.class);
        final ArithmeticExpressionMultiplyAdd arithmeticexpressionmultiplyadd = context.getBean(ArithmeticExpressionMultiplyAdd.class);
        arithmeticexpressionmultiplyadd.procedureDivision();
    }
}
