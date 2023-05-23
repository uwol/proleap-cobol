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
public class SemDe {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: EXAMPLE                                                                            //   (2)  PROGRAM-ID. EXAMPLE.
    */
                                                                                                     //   (3)  
    @Inject                                                                                          //   (4)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (5)  WORKING-STORAGE SECTION.
    public class COMPANYType {                                                                       //   (6)  01 COMPANY.
        @Size(max=60)                                                                                //   (7)     05 COMPANY-NAME PIC X(60) VALUE "Semantic Designs".
        protected String company_name = "Semantic Designs";                                         
        public class Company_addressType {                                                           //   (8)     05 COMPANY-ADDRESS.
            @Size(max=80)                                                                            //   (9)        10 STREET PIC X(80) VALUE "13171 Pond Springs Rd.".
            protected String street = "13171 Pond Springs Rd.";                                     
            public class CITYType {                                                                  //  (10)        10 CITY.
                @Size(max=40)                                                                        //  (11)    15 CITY-NAME PIC X(40) VALUE "Austin".
                protected String city_name = "Austin";                                              
                @Size(max=2)                                                                         //  (12)    15 FILLER PIC XX VALUE ", ".
                protected String filler0 = ", ";                                                    
                @Size(max=2)                                                                         //  (13)    15 CITY-STATE PIC XX VALUE "TX".
                protected String city_state = "TX";                                                 
                public class ZIPType {                                                               //  (14)           15 ZIP.
                    @Digits(integer=5, fraction=0)                                                   //  (15)              20 ZIP-5  PIC 9(5) VALUE 78729.
                    protected BigDecimal zip_5 = BigDecimal.valueOf(78729);                         
                    @Size(max=1)                                                                     //  (16)              20 FILLER PIC X    VALUE "-".
                    protected String filler1 = "-";                                                 
                    @Digits(integer=4, fraction=0)                                                   //  (17)              20 ZIP-E4 PIC 9(4) VALUE 7102.
                    protected BigDecimal zip_e4 = BigDecimal.valueOf(7102);                         
                }
                
                protected ZIPType zip = new ZIPType();
            }
            
            protected CITYType city = new CITYType();
        }
        
        protected Company_addressType company_address = new Company_addressType();
    }
    
    protected COMPANYType company = new COMPANYType();
    public class Line_itemType {                                                                     //  (18)  01 LINE-ITEM.
        @Size(max=20)                                                                                //  (19)     05 ITEM PIC X(20)     VALUE "Item Description".
        protected String item = "Item Description";                                                 
        @Digits(integer=3, fraction=0)                                                               //  (20)     05 QUANTITY PIC 999   VALUE 217.
        protected BigDecimal quantity = BigDecimal.valueOf(217);                                    
        @Digits(integer=4, fraction=2)                                                               //  (21)     05 PRICE PIC 9999V99  VALUE 24.95.
        protected BigDecimal price = BigDecimal.valueOf(24.95);                                     
    }
    
    protected Line_itemType line_item = new Line_itemType();
                                                                                                     //  (22)  
    @Digits(integer=6, fraction=2)                                                                   //  (23)  77 TOTAL-AMOUNT PIC 999999V99.
    protected BigDecimal total_amount;                                                              
    @Digits(integer=6, fraction=2)                                                                   //  (24)  77 DISCOUNT-THRESHOLD PIC 999999V99 VALUE 1111.11.
    protected BigDecimal discount_threshold = BigDecimal.valueOf(1111.11);                          
    @Digits(integer=2, fraction=0)                                                                   //  (25)  77 DISCOUNT-PERCENT   PIC 99 VALUE 20.
    protected BigDecimal discount_percent = BigDecimal.valueOf(20);                                 
    @Digits(integer=8, fraction=2)                                                                   //  (26)  77 DISCOUNT-AMOUNT    PIC 99999999V99.
    protected BigDecimal discount_amount;                                                           
                                                                                                     //  (27)  
                                                                                                     //  (28)  PROCEDURE DIVISION.
    public void perform_task() throws Exception {                                                    //  (29)  PERFORM-TASK.
        compute_total();                                                                             //  (30)    PERFORM COMPUTE-TOTAL.
        display_total();                                                                             //  (31)    PERFORM DISPLAY-TOTAL.
        System.exit(0);                                                                              //  (32)    STOP RUN.
    }
    
    public void compute_total() throws Exception {                                                   //  (33)  COMPUTE-TOTAL.
        total_amount = quantity.line_item.multiply(price.line_item);                                 //  (34)    MULTIPLY QUANTITY BY PRICE GIVING TOTAL-AMOUNT.
        if (total_amount.compareTo(discount_threshold) > 0) {                                        //  (35)    IF TOTAL-AMOUNT > DISCOUNT-THRESHOLD
                                                                                                     //  (36)      MULTIPLY TOTAL-AMOUNT BY DISCOUNT-PERCENT
            discount_amount = total_amount.multiply(discount_percent);                               //  (37)        GIVING DISCOUNT-AMOUNT
            discount_amount = discount_amount.divide(BigDecimal.valueOf(100));                       //  (38)      DIVIDE 100 INTO DISCOUNT-AMOUNT
            total_amount = total_amount.subtract(discount_amount);                                   //  (39)      SUBTRACT DISCOUNT-AMOUNT FROM TOTAL-AMOUNT.
        } 
    }
    
    public void display_total() throws Exception {                                                   //  (40)  DISPLAY-TOTAL.
        System.out.println(company_name.company);                                                    //  (41)    DISPLAY COMPANY-NAME.
        System.out.print("Total: ");                                                                 //  (42)    DISPLAY "Total: ", TOTAL-AMOUNT.
        System.out.print(total_amount);                                                             
        System.out.println();                                                                       
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, SemDe.class);
        final SemDe semde = context.getBean(SemDe.class);
        semde.perform_task();
    }
}
