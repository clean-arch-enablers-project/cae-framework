package utils.normal_ports;

import com.cae.ports.FunctionPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeNormalFunctionPort extends FunctionPort<String, String> {

    @Override
    protected String executeLogic(String input, ExecutionContext correlation) {
        return "";
    }

}
