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
public class AddToGivingStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ADDSTMT                                                                            //   (2)  PROGRAM-ID. ADDSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=10, fraction=0)                                                                  //   (5)  77 TEST1 PIC 9(10).
    protected BigDecimal test1;                                                                     
    @Digits(integer=10, fraction=0)                                                                  //   (6)  77 TEST2 PIC 9(10).
    protected BigDecimal test2;                                                                     
    @Digits(integer=10, fraction=0)                                                                  //   (7)  77 TEST3 PIC 9(10).
    protected BigDecimal test3;                                                                     
    @Digits(integer=10, fraction=0)                                                                  //   (8)  77 TEST4 PIC 9(10).
    protected BigDecimal test4;                                                                     
    @Digits(integer=10, fraction=0)                                                                  //   (9)  77 TEST5 PIC 9(10).
    protected BigDecimal test5;                                                                     
    public void procedureDivision() throws Exception{                                                //  (10)  PROCEDURE DIVISION.
        test5 = test1.add(BigDecimal.ONE);                                                           //  (11)      ADD 1 TO TEST1 GIVING TEST5.
        test4 = test2.add(test3).add(BigDecimal.ONE).add(BigDecimal.valueOf(2));                     //  (12)      ADD 1 2 TO TEST2 TEST3 GIVING TEST4 TEST5.
        test5 = test2.add(test3).add(BigDecimal.ONE).add(BigDecimal.valueOf(2));                    
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, AddToGivingStatement.class);
        final AddToGivingStatement addtogivingstatement = context.getBean(AddToGivingStatement.class);
        addtogivingstatement.procedureDivision();
    }
}
