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
public class DataDescriptionFillerNested {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCRFILLERNESTED                                                              //   (2)  PROGRAM-ID. DATADESCRFILLERNESTED.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class Filler0Type {                                                                       //   (5)  01 FILLER.
        @Size(max=20)                                                                                //   (6)     05 FILLER PIC X(20) VALUE 'SOME-FILLER-VALUE'.
        protected String filler1 = "SOME-FILLER-VALUE";                                             
        @Size(max=20)                                                                                //   (7)     05 SOMEID PIC X(20) VALUE 'SOME-VALUE'.
        protected String someid = "SOME-VALUE";                                                     
    }
    
    protected Filler0Type filler0 = new Filler0Type();
}
