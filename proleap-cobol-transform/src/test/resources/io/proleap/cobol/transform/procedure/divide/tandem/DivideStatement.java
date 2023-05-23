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
public class DivideStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DIVSTMT                                                                            //   (2)  PROGRAM-ID. DIVSTMT.
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
    @Digits(integer=10, fraction=0)                                                                  //   (9)  77 SOMEID11 PIC 9(10).
    protected BigDecimal someid11;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (10)  77 SOMEID12 PIC 9(10).
    protected BigDecimal someid12;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (11)  77 SOMEID13 PIC 9(10).
    protected BigDecimal someid13;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (12)  77 SOMEID14 PIC 9(10).
    protected BigDecimal someid14;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (13)  77 SOMEID21 PIC 9(10).
    protected BigDecimal someid21;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (14)  77 SOMEID22 PIC 9(10).
    protected BigDecimal someid22;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (15)  77 SOMEID23 PIC 9(10).
    protected BigDecimal someid23;                                                                  
    @Digits(integer=10, fraction=0)                                                                  //  (16)  77 SOMEID24 PIC 9(10).
    protected BigDecimal someid24;                                                                  
    public void procedureDivision() throws Exception{                                                //  (17)  PROCEDURE DIVISION.
        someid3 = someid2.divide(someid1);                                                           //  (18)     DIVIDE SOMEID1 INTO SOMEID2 GIVING SOMEID3 ROUNDED REMAINDER SOMEID4.
        someid12 = someid12.divide(someid11);                                                        //  (19)     DIVIDE SOMEID11 INTO SOMEID12 ROUNDED SOMEID13 REMAINDER SOMEID14.
        someid13 = someid13.divide(someid11);                                                       
        someid23 = someid21.divide(someid22);                                                        //  (20)     DIVIDE SOMEID21 BY SOMEID22 GIVING SOMEID23 ROUNDED REMAINDER SOMEID24.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, DivideStatement.class);
        final DivideStatement dividestatement = context.getBean(DivideStatement.class);
        dividestatement.procedureDivision();
    }
}
