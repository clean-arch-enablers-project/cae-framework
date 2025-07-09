package utils.problematic_ports;

import com.cae.ports.ConsumerPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicConsumerPort extends ConsumerPort<String> {
    @Override
    protected void executeLogic(String input, ExecutionContext correlation) {
        throw new RuntimeException("Yeah... just exploded.");
    }
}
