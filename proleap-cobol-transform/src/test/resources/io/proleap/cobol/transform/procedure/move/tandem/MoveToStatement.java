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
public class MoveToStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: MOVETOSTMT                                                                         //   (2)  PROGRAM-ID. MOVETOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=9)                                                                                     //   (5)  77 SOME-TEXT PICTURE IS X(9).
    protected String some_text;                                                                     
    @Size(max=9)                                                                                     //   (6)  77 SOME-TEXT2 PICTURE IS X(9).
    protected String some_text2;                                                                    
    @Digits(integer=2, fraction=0)                                                                   //   (7)  77 SOME-NUMBER PIC 99 COMP.
    protected BigDecimal some_number;                                                               
    public void procedureDivision() throws Exception{                                                //   (8)  PROCEDURE DIVISION.
        some_text = "Test";                                                                          //   (9)      MOVE "Test" TO SOME-TEXT.
        some_number = BigDecimal.ONE;                                                                //  (10)      MOVE 1 TO SOME-NUMBER.
        some_text2 = some_text;                                                                      //  (11)      MOVE SOME-TEXT TO SOME-TEXT2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, MoveToStatement.class);
        final MoveToStatement movetostatement = context.getBean(MoveToStatement.class);
        movetostatement.procedureDivision();
    }
}
