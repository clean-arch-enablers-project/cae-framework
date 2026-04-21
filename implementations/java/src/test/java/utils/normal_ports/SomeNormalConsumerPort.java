package utils.normal_ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.ConsumerPort;

public class SomeNormalConsumerPort extends ConsumerPort<String> {
    @Override
    protected void executeLogic(String input, ExecutionContext correlation) {

    }
}
