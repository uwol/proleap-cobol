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
public class SetToStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: SETTOSTMT                                                                          //   (2)  PROGRAM-ID. SETTOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=9)                                                                                     //   (5)  77 SOMEID1 PICTURE IS X(9).
    protected String someid1;                                                                       
    @Size(max=9)                                                                                     //   (6)  77 SOMEID2 PICTURE IS X(9).
    protected String someid2;                                                                       
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        someid1 = "test";                                                                            //   (8)     SET SOMEID1 TO 'test'.
        someid2 = String.valueOf(true);                                                              //   (9)     SET SOMEID2 TO True.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, SetToStatement.class);
        final SetToStatement settostatement = context.getBean(SetToStatement.class);
        settostatement.procedureDivision();
    }
}
