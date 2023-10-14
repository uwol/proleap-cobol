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
public class AddCorrespondingStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: ADDSTMT                                                                            //   (2)  PROGRAM-ID. ADDSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Digits(integer=10, fraction=0)                                                                  //   (5)  77 TEST1 PIC 9(10).
    protected BigDecimal test1;                                                                     
    @Digits(integer=10, fraction=0)                                                                  //   (6)  77 TEST2 PIC 9(10).
    protected BigDecimal test2;                                                                     
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        test2.add(test1);                                                                            //   (8)      ADD CORRESPONDING TEST1 TO TEST2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, AddCorrespondingStatement.class);
        final AddCorrespondingStatement addcorrespondingstatement = context.getBean(AddCorrespondingStatement.class);
        addcorrespondingstatement.procedureDivision();
    }
}
