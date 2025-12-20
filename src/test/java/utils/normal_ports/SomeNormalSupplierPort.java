package utils.normal_ports;

import com.cae.framework.ports.SupplierPort;
import com.cae.context.ExecutionContext;

public class SomeNormalSupplierPort extends SupplierPort<String> {

    @Override
    protected String executeLogic(ExecutionContext correlation) {
        return "";
    }

}
