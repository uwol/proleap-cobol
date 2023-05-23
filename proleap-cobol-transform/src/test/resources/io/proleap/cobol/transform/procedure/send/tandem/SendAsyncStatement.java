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
public class SendAsyncStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: TERMINATESTMT                                                                      //   (2)  PROGRAM-ID. TERMINATESTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=10, fraction=0)                                                                  //   (5)  77 SOMEID1 PIC 9(10).
    protected BigDecimal someid1;                                                                   
    public void procedureDivision() throws Exception{                                                //   (6)  PROCEDURE DIVISION.
        try {
        } catch (Exception e) {
                                                                                                     //   (7)     SEND TO TOP SOMEID1
            System.out.println("Test");                                                              //   (8)        ON EXCEPTION DISPLAY "Test".
        }                                                                                           
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, SendAsyncStatement.class);
        final SendAsyncStatement sendasyncstatement = context.getBean(SendAsyncStatement.class);
        sendasyncstatement.procedureDivision();
    }
}
