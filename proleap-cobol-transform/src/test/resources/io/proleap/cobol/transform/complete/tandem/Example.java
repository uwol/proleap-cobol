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
public class Example {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: EXAMPLE                                                                            //   (2)  PROGRAM-ID. EXAMPLE.
    */
    @Inject                                                                                          //   (3)  DATA DIVISION.
    EntityService entityService;
    
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class Some_itemType {                                                                     //   (5)  01 SOME-ITEM.
        @Size(max=20)                                                                                //   (6)     05 ITEM-NAME PIC X(20) VALUE "Item Name".
        protected String item_name = "Item Name";                                                   
        @Digits(integer=3, fraction=2)                                                               //   (7)     05 PRICE PIC 999V99 VALUE 99.99.
        protected BigDecimal price = BigDecimal.valueOf(99.99);                                     
        @Digits(integer=3, fraction=0)                                                               //   (8)     05 AMOUNT PIC 999 VALUE 42.
        protected BigDecimal amount = BigDecimal.valueOf(42);                                       
    }
    
    protected Some_itemType some_item = new Some_itemType();
    public class Some_personType {                                                                   //   (9)  01 SOME-PERSON.
        @Size(max=20)                                                                                //  (10)     05 PERSON-NAME PIC X(20) VALUE "Grace Hopper".
        protected String person_name = "Grace Hopper";                                              
        public class Person_addressType {                                                            //  (11)     05 PERSON-ADDRESS.
            @Size(max=20)                                                                            //  (12)        10 STREET PIC X(20).
            protected String street;                                                                
            protected String city = "Arlington";                                                     //  (13)        10 CITY VALUE "Arlington".
        }
        
        protected Person_addressType person_address = new Person_addressType();
    }
    
    protected Some_personType some_person = new Some_personType();
    @Digits(integer=5, fraction=2)                                                                   //  (14)  77 TOTAL-AMOUNT          PIC 99999V99.
    protected BigDecimal total_amount;                                                              
    @Digits(integer=5, fraction=2)                                                                   //  (15)  77 DISCOUNT-BOUNDARY     PIC 99999V99 VALUE 1000.00.
    protected BigDecimal discount_boundary = BigDecimal.valueOf(1000.00);                           
    @Digits(integer=2, fraction=0)                                                                   //  (16)  77 DISCOUNT-PERCENT      PIC 99 VALUE 10.
    protected BigDecimal discount_percent = BigDecimal.TEN;                                         
    @Digits(integer=5, fraction=2)                                                                   //  (17)  77 DISCOUNT-AMOUNT       PIC 99999V99.
    protected BigDecimal discount_amount;                                                           
                                                                                                     //  (18)  
                                                                                                     //  (19)  PROCEDURE DIVISION.
    public void batch_discount() throws Exception {                                                  //  (20)  BATCH-DISCOUNT.
        compute_discount();                                                                          //  (21)    PERFORM COMPUTE-DISCOUNT.
        display_discount();                                                                          //  (22)    PERFORM DISPLAY-DISCOUNT.
        System.exit(0);                                                                              //  (23)    STOP RUN.
    }
    
    public void compute_discount() throws Exception {                                                //  (24)  COMPUTE-DISCOUNT.
        total_amount = amount.some_item.multiply(price.some_item);                                   //  (25)    MULTIPLY AMOUNT BY PRICE GIVING TOTAL-AMOUNT.
        if (total_amount.compareTo(discount_boundary) > 0) {                                         //  (26)    IF TOTAL-AMOUNT > DISCOUNT-BOUNDARY
            discount_amount = total_amount.multiply(discount_percent);                               //  (27)      MULTIPLY TOTAL-AMOUNT BY DISCOUNT-PERCENT GIVING DISCOUNT-AMOUNT
            discount_amount = discount_amount.divide(BigDecimal.valueOf(100));                       //  (28)      DIVIDE 100 INTO DISCOUNT-AMOUNT
            total_amount = total_amount.subtract(discount_amount);                                   //  (29)      SUBTRACT DISCOUNT-AMOUNT FROM TOTAL-AMOUNT.
        } 
    }
    
    public void display_discount() throws Exception {                                                //  (30)  DISPLAY-DISCOUNT.
        System.out.println(person_name.some_person);                                                 //  (31)    DISPLAY PERSON-NAME.
        System.out.print("Total: ");                                                                 //  (32)    DISPLAY "Total: ", TOTAL-AMOUNT.
        System.out.print(total_amount);                                                             
        System.out.println();                                                                       
        System.out.print("Discount: ");                                                              //  (33)    DISPLAY "Discount: ", DISCOUNT-AMOUNT.
        System.out.print(discount_amount);                                                          
        System.out.println();                                                                       
    }
    
    
    public static void main(String[] args) throws Exception {
        final ApplicationContext context = new AnnotationConfigApplicationContext(ProLeapCobolApiSpringConfig.class, Example.class);
        final Example example = context.getBean(Example.class);
        example.batch_discount();
    }
}
