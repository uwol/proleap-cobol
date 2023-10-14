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
public class MoveCorrespondingToStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: MOVECORRTOSTMT                                                                     //   (2)  PROGRAM-ID. MOVECORRTOSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=9)                                                                                     //   (5)  77 SOME-TEXT PICTURE IS X(9).
    protected String some_text;                                                                     
    @Size(max=9)                                                                                     //   (6)  77 SOME-TEXT2 PICTURE IS X(9).
    protected String some_text2;                                                                    
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        some_text2 = some_text;                                                                      //   (8)      MOVE CORRESPONDING SOME-TEXT TO SOME-TEXT2.
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, MoveCorrespondingToStatement.class);
        final MoveCorrespondingToStatement movecorrespondingtostatement = context.getBean(MoveCorrespondingToStatement.class);
        movecorrespondingtostatement.procedureDivision();
    }
}
