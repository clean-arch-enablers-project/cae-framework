package utils.normal_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.FunctionPort;

public class SomeNormalFunctionPort extends FunctionPort<String, String> {

    @Override
    protected String executeLogic(String input, ExecutionContext correlation) {
        return "";
    }

}
