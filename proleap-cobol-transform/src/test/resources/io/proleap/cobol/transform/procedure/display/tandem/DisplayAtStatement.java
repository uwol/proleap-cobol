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
public class DisplayAtStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DISPLAYATSTMT                                                                      //   (2)  PROGRAM-ID. DISPLAYATSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=9)                                                                                     //   (5)  77 SOMEID1 PICTURE IS X(9).
    protected String someid1;                                                                       
    @Size(max=1)                                                                                     //   (6)  77 SOMEID2 PICTURE IS X(1).
    protected String someid2;                                                                       
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        System.out.print(someid1);                                                                   //   (8)     DISPLAY SOMEID1 '2' AT SOMEID2.
        System.out.print("2");                                                                      
        System.out.println();                                                                       
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, DisplayAtStatement.class);
        final DisplayAtStatement displayatstatement = context.getBean(DisplayAtStatement.class);
        displayatstatement.procedureDivision();
    }
}
