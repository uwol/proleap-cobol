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
public class ExecSqlStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ExecSql                                                                            //   (2)  PROGRAM-ID. ExecSql.
    */
    public void procedureDivision() throws Exception{                                                //   (3)  PROCEDURE DIVISION.
        java.sql.DriverManager.getConnection("").prepareStatement("EXEC SQL SELECT * FROM customers END-EXEC").executeQuery(); //   (4) *>EXECSQL EXEC SQL SELECT * FROM customers END-EXEC }.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, ExecSqlStatement.class);
        final ExecSqlStatement execsqlstatement = context.getBean(ExecSqlStatement.class);
        execsqlstatement.procedureDivision();
    }
}
