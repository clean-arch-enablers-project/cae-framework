package utils.problematic_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.RunnablePort;

public class SomeProblematicRunnablePort extends RunnablePort {

    @Override
    protected void executeLogic(ExecutionContext correlation) {
        throw new RuntimeException("ops!!");
    }

}
