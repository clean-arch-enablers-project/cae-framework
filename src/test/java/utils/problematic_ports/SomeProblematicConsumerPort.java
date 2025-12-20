package utils.problematic_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.ConsumerPort;

public class SomeProblematicConsumerPort extends ConsumerPort<String> {
    @Override
    protected void executeLogic(String input, ExecutionContext correlation) {
        throw new RuntimeException("Yeah... just exploded.");
    }
}
