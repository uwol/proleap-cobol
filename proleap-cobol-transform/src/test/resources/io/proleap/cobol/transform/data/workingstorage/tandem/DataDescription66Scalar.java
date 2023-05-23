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
public class DataDescription66Scalar {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCR77                                                                        //   (2)  PROGRAM-ID. DATADESCR77.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class ITEMSType {                                                                         //   (5)  01 ITEMS.
        @Size(max=10)                                                                                //   (6)     02 ITEM1 PIC X(10).
        protected String item1;                                                                     
        @Size(max=10)                                                                                //   (7)     02 ITEM2 PIC X(10).
        protected String item2;                                                                     
        @Size(max=10)                                                                                //   (8)     02 ITEM3 PIC X(10).
        protected String item3;                                                                     
        @Size(max=10)                                                                                //   (9)     02 ITEM4 PIC X(10).
        protected String item4;                                                                     
    }
    
    protected ITEMSType items = new ITEMSType();
}
