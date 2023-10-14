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
public class AcceptStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ACCEPTSTMT                                                                         //   (2)  PROGRAM-ID. ACCEPTSTMT.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        try {
        } catch (Exception e) {
                                                                                                     //   (4)      ACCEPT TEST1 FROM TODAYS-NAME
            System.out.println("Test");                                                              //   (5)         ON EXCEPTION DISPLAY "Test".
        }                                                                                           
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, AcceptStatement.class);
        final AcceptStatement acceptstatement = context.getBean(AcceptStatement.class);
        acceptstatement.procedureDivision();
    }
}
