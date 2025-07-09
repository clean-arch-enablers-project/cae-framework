package utils.problematic_ports;

import com.cae.ports.FunctionPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicFunctionPort extends FunctionPort<String, String> {
    @Override
    protected String executeLogic(String input, ExecutionContext correlation) {
        throw new RuntimeException("oh nooo!!");
    }
}
