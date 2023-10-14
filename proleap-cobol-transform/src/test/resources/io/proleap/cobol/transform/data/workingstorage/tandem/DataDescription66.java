import java.lang.String;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Inject;

import io.proleap.cobol.api.*;
import io.proleap.cobol.api.environment.inputoutput.filecontrol.*;
import com.google.inject.Guice;
import com.google.inject.Injector;

public class DataDescription66 {
    /**                                                                                              //   (1)  IDENTIFICATION DIVISION.
    * Program-Id: DATADESCR77                                                                        //   (2)  PROGRAM-ID. DATADESCR77.
    */
                                                                                                     //   (3)  DATA DIVISION.
                                                                                                     //   (4)  WORKING-STORAGE SECTION.
    public class ITEMSType {                                                                         //   (5)  01 ITEMS.
        public String item1;                                                                         //   (6)     02 ITEM1 PIC X(10).
        public String item2;                                                                         //   (7)     02 ITEM2 PIC X(10).
        public String item3;                                                                         //   (8)     02 ITEM3 PIC X(10).
        public String item4;                                                                         //   (9)     02 ITEM4 PIC X(10).
    }
    public ITEMSType items = new ITEMSType();
}
