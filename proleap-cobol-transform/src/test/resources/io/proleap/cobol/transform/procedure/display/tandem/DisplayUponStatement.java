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
public class DisplayUponStatement {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DISPLAYSTMT                                                                        //   (2)  PROGRAM-ID. DISPLAYSTMT.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    @Size(max=9)                                                                                     //   (5)  77 SOMEID1 PICTURE IS X(9).
    protected String someid1;                                                                       
    @Digits(integer=1, fraction=0)                                                                   //   (6)  77 SOMEENV1 PICTURE IS 9(1).
    protected BigDecimal someenv1;                                                                  
    public void procedureDivision() throws Exception{                                                //   (7)  PROCEDURE DIVISION.
        System.out.print(someid1);                                                                   //   (8)     DISPLAY SOMEID1 '2' 3 UPON SOMEENV1 WITH NO ADVANCING.
        System.out.print("2");                                                                      
        System.out.print(BigDecimal.valueOf(3));                                                    
        System.out.println();                                                                       
    }
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, DisplayUponStatement.class);
        final DisplayUponStatement displayuponstatement = context.getBean(DisplayUponStatement.class);
        displayuponstatement.procedureDivision();
    }
}
