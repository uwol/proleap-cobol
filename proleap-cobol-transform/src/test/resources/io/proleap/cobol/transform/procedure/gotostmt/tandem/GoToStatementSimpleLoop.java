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
public class GoToStatementSimpleLoop {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: GOTOLOOPSTMT                                                                       //   (2)  PROGRAM-ID. GOTOLOOPSTMT.
    */
                                                                                                     //   (3)  PROCEDURE DIVISION.
    public void someproc1() throws Exception {                                                       //   (4)  SOMEPROC1.
        System.out.println("test");                                                                  //   (5)     DISPLAY 'test'.
        someproc1();                                                                                 //   (6)     GO TO SOMEPROC1.
    }
    
    public void someproc2() throws Exception {                                                       //   (7)  SOMEPROC2.
        System.out.println("test2");                                                                 //   (8)     DISPLAY 'test2'.
        someproc1();                                                                                 //   (9)     GO TO SOMEPROC1.
        throw new RuntimeException("someproc1 must terminate due to GO TO in call stack.");
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, GoToStatementSimpleLoop.class);
        final GoToStatementSimpleLoop gotostatementsimpleloop = context.getBean(GoToStatementSimpleLoop.class);
        gotostatementsimpleloop.someproc1();
    }
}
