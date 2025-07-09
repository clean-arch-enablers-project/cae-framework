package utils.problematic_ports;

import com.cae.ports.SupplierPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicSupplierPort extends SupplierPort<String> {

    @Override
    protected String executeLogic(ExecutionContext correlation) {
        throw new RuntimeException("oh no!");
    }
}
