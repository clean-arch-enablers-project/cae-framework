package utils.problematic_ports;

import com.cae.ports.RunnablePort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicRunnablePort extends RunnablePort {

    @Override
    protected void executeLogic(ExecutionContext correlation) {
        throw new RuntimeException("ops!!");
    }

}
