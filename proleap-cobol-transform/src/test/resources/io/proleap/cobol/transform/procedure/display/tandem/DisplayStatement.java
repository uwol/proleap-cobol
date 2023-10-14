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
public class DisplayStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PRECEDENCE                                                                         //   (2)  PROGRAM-ID. PRECEDENCE.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        System.out.println("Hello World");                                                           //   (4)     DISPLAY "Hello World".
        System.exit(0);                                                                              //   (5)     STOP RUN.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, DisplayStatement.class);
        final DisplayStatement displaystatement = context.getBean(DisplayStatement.class);
        displaystatement.procedureDivision();
    }
}
