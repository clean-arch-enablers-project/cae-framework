package utils.problematic_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.SupplierPort;

public class SomeProblematicSupplierPort extends SupplierPort<String> {

    @Override
    protected String executeLogic(ExecutionContext correlation) {
        throw new RuntimeException("oh no!");
    }
}
