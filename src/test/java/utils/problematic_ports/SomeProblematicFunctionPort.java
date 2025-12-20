package utils.problematic_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.FunctionPort;

public class SomeProblematicFunctionPort extends FunctionPort<String, String> {
    @Override
    protected String executeLogic(String input, ExecutionContext correlation) {
        throw new RuntimeException("oh nooo!!");
    }
}
