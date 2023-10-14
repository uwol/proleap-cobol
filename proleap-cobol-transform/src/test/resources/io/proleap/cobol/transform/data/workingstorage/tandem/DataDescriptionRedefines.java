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
public class DataDescriptionRedefines {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCRREDEFINES                                                                 //   (2)  PROGRAM-ID. DATADESCRREDEFINES.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class PERSONType {                                                                        //   (5)  01 PERSON.
        @Size(max=8)                                                                                 //   (6)     05 BIRTHDATE PIC X(8).
        protected String birthdate;                                                                 
        public class Birthdate_partsType {                                                           //   (7)     05 BIRTHDATE-PARTS REDEFINES BIRTHDATE.
            @Digits(integer=4, fraction=0)                                                           //   (8)        07 BIRTH-YEAR PIC 9(4).
            protected BigDecimal birth_year;                                                        
            @Digits(integer=2, fraction=0)                                                           //   (9)        07 BIRTH-MONTH PIC 9(2).
            protected BigDecimal birth_month;                                                       
            @Digits(integer=2, fraction=0)                                                           //  (10)        07 BIRTH-DAY PIC 9(2).
            protected BigDecimal birth_day;                                                         
        }
        
        protected Birthdate_partsType birthdate_parts = new Birthdate_partsType();
    }
    
    protected PERSONType person = new PERSONType();
}
