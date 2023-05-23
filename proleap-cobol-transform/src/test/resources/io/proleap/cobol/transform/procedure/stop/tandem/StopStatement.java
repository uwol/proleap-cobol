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
public class StopStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: STOPSTMT                                                                           //   (2)  PROGRAM-ID. STOPSTMT.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        System.exit(0);                                                                              //   (4)     STOP RUN.
        System.out.println("someliteral");                                                           //   (5)     STOP 'someliteral'.
        System.exit(0);                                                                             
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, StopStatement.class);
        final StopStatement stopstatement = context.getBean(StopStatement.class);
        stopstatement.procedureDivision();
    }
}
