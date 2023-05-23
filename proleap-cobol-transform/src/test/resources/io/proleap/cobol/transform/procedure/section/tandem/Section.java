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
public class Section {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: SCTN                                                                               //   (2)  PROGRAM-ID. SCTN.
    */
                                                                                                     //   (3)  PROCEDURE DIVISION.
    public void some_section() throws Exception {                                                    //   (4)     SOME-SECTION SECTION.
        System.out.println("Hello World");                                                           //   (5)        DISPLAY "Hello World".
        System.exit(0);                                                                              //   (6)        STOP RUN.
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, Section.class);
        final Section section = context.getBean(Section.class);
        section.some_section();
    }
}
