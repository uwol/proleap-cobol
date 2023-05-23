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
public class ExitStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: EXITSTMT                                                                           //   (2)  PROGRAM-ID. EXITSTMT.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        System.exit(0);                                                                              //   (4)     EXIT.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ExitStatement.class);
        final ExitStatement exitstatement = context.getBean(ExitStatement.class);
        exitstatement.procedureDivision();
    }
}
