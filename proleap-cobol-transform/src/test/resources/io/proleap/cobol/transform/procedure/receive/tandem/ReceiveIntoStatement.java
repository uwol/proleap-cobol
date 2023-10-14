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
public class ReceiveIntoStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: RECEIVEINTOSTMT                                                                    //   (2)  PROGRAM-ID. RECEIVEINTOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=10)                                                                                    //   (5)  77 SOMECD1 PIC X(10).
    protected String somecd1;                                                                       
    @Digits(integer=10, fraction=0)                                                                  //   (6)  77 SOMEID1 PIC 9(10).
    protected BigDecimal someid1;                                                                   
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        try {
        } catch (Exception e) {
                                                                                                     //   (8)     RECEIVE SOMEID1 MESSAGE INTO SOMECD1
            System.out.println("Test");                                                              //   (9)        ON EXCEPTION DISPLAY "Test".
        }                                                                                           
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ReceiveIntoStatement.class);
        final ReceiveIntoStatement receiveintostatement = context.getBean(ReceiveIntoStatement.class);
        receiveintostatement.procedureDivision();
    }
}
