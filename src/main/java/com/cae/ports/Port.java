package com.cae.ports;

import com.cae.ports.auto_logging.PortInsightsManager;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.Getter;

/**
 * Ports are meant to be the bridge between your core components
 * and the outside world. Ports come in a couple flavours: functions,
 * consumers, suppliers and runnables. Those subtypes are available
 * as specifics.
 */
@Getter
public abstract class Port {

    /**
     * Name of the implementation. If you need to find something
     * by ID within some use case implementation, accessing a database
     * or an API for that purpose, you would call a port from there instead
     * of calling the finding-by-id method implementation. The name of
     * the class would be set here, in this attribute, based on the class name
     * itself.
     */
    protected final String name;

    protected Port() {
        this.name = this.getClass().getSimpleName();
    }


}
