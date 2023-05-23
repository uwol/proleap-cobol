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
public class GoToStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: GOTOSTMT                                                                           //   (2)  PROGRAM-ID. GOTOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=1, fraction=0)                                                                   //   (5)     01 SOMEDATA1 PIC 9.
    protected BigDecimal somedata1;                                                                 
                                                                                                     //   (6)  PROCEDURE DIVISION.
    public void someproc1() throws Exception {                                                       //   (7)  SOMEPROC1.
        someproc2();                                                                                 //   (8)     GO TO SOMEPROC2.
        throw new RuntimeException("someproc2 must terminate due to GO TO in call stack.");
    }
    
    public void someproc2() throws Exception {                                                       //   (9)  SOMEPROC2.
        if(BigDecimal.valueOf(1).equals(somedata1)){
            someproc3();                                                                             //  (10)     GO TO SOMEPROC3 SOMEPROC4 SOMEPROC5 DEPENDING ON SOMEDATA1.
            throw new RuntimeException("someproc3 must terminate due to GO TO in call stack.");
        }
        if(BigDecimal.valueOf(2).equals(somedata1)){
            someproc4();                                                                            
            throw new RuntimeException("someproc4 must terminate due to GO TO in call stack.");
        }
        if(BigDecimal.valueOf(3).equals(somedata1)){
            someproc5();                                                                            
            throw new RuntimeException("someproc5 must terminate due to GO TO in call stack.");
        }
    }
    
    public void someproc3() throws Exception {                                                       //  (11)  SOMEPROC3.
        System.out.println("Proc3");                                                                 //  (12)     Display "Proc3".
    }
    
    public void someproc4() throws Exception {                                                       //  (13)  SOMEPROC4.
        System.out.println("Proc4");                                                                 //  (14)     Display "Proc4".
    }
    
    public void someproc5() throws Exception {                                                       //  (15)  SOMEPROC5.
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, GoToStatement.class);
        final GoToStatement gotostatement = context.getBean(GoToStatement.class);
        gotostatement.someproc1();
    }
}
