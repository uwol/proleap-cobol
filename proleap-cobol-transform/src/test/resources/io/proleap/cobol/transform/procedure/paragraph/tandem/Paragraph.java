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
public class Paragraph {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: PARAGRAPH                                                                          //   (2)  PROGRAM-ID. PARAGRAPH.
    */
                                                                                                     //   (3)  PROCEDURE DIVISION.
    public void init() throws Exception {                                                            //   (4)  INIT.
        System.exit(0);                                                                              //   (5)      STOP RUN.
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, Paragraph.class);
        final Paragraph paragraph = context.getBean(Paragraph.class);
        paragraph.init();
    }
}
