package utils.normal_ports;

import com.cae.ports.SupplierPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeNormalSupplierPort extends SupplierPort<String> {

    @Override
    protected String executeLogic(ExecutionContext correlation) {
        return "";
    }

}
