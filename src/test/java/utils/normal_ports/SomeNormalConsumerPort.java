package utils.normal_ports;

import com.cae.ports.ConsumerPort;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeNormalConsumerPort extends ConsumerPort<String> {
    @Override
    protected void executeLogic(String input, ExecutionContext correlation) {

    }
}
