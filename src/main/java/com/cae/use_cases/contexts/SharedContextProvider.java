package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;

import java.util.HashMap;
import java.util.Map;

/**
 * This class serves the purpose of providing global, stateless, singleton objects
 * to all {@link ExecutionContext} instances.
 * <p>
 * Once created, the shared context becomes accessible to any part of the application
 * that has access to an {@code ExecutionContext}, allowing cross-cutting concerns
 * (e.g., loggers, metrics collectors, feature toggles) to be centrally registered and reused.
 * <p>
 * It is recommended to call the {@link #lock()} method after all shared objects have been registered,
 * to prevent accidental mutation after the application bootstrap phase.
 */
@Getter
public class SharedContextProvider {

    public static final SharedContextProvider SINGLETON = new SharedContextProvider();

    private boolean isLocked = false;
    private final Map<String, Object> sharedContext = new HashMap<>();

    /**
     * Registers a shared object under the given key.
     *
     * @param key the name under which the object will be stored
     * @param sharedObject the object to be shared
     * @return the same {@code SharedContextProvider} instance for chaining
     * @throws IllegalStateException if this context has already been locked
     * @throws InternalMappedException if the given key is already registered
     */
    public SharedContextProvider put(String key, Object sharedObject){
        if (this.isLocked)
            throw new IllegalStateException("Cannot add shared context objects after it is locked");
        if (this.sharedContext.containsKey(key))
            throw new InternalMappedException("Key already provided for the shared context", "The key already used: '" + key + "'");
        this.sharedContext.put(key, sharedObject);
        return this;
    }

    /**
     * Locks the shared context to prevent any further modifications.
     * <p>
     * This should be called once all desired shared objects have been registered,
     * typically during the application bootstrap phase.
     */
    public void lock(){
        this.isLocked = true;
    }

}
