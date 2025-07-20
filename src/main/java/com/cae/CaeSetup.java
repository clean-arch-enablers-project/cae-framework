package com.cae;

import com.cae.autofeatures.autoauth.RoleRetriever;
import com.cae.autofeatures.autoauth.RoleRetrieverRegistry;
import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.IOAutologMode;
import com.cae.autofeatures.autolog.Logger;
import com.cae.autofeatures.autometrics.AutometricsProvider;
import com.cae.autofeatures.autometrics.AutometricsSubscriber;
import com.cae.autofeatures.autonotify.AutonotifyProvider;
import com.cae.autofeatures.autonotify.AutonotifySubscriber;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.properties.Properties;
import com.cae.properties.Property;
import com.cae.use_cases.contexts.SharedContextProvider;
import lombok.RequiredArgsConstructor;

/**
 * Centralized entry point for configuring the CAE framework.
 * <p>
 * The {@code CaeSetup} class provides a fluent API to configure all the core Autofeatures
 * (Autolog, Autometrics, Autonotify, Autoauth, etc.) as well as the shared context registry.
 * <p>
 * Example usage:
 * <pre>{@code
 * CaeSetup.SINGLETON
 *     .autolog()
 *         .setProvidedInstance(myLogger)
 *         .setAsync(true)
 *         .done()
 *     .autometrics()
 *         .setAsync(false)
 *         .subscribe(mySubscriber)
 *         .done();
 * }</pre>
 * This class is implemented as a singleton. Always use {@code CaeSetup.SINGLETON} to access the configuration interface.
 */
public class CaeSetup {

    public static final CaeSetup SINGLETON = new CaeSetup();

    /**
     * Begins configuration of the Autolog Autofeature.
     *
     * @return a fluent builder for {@link AutologSetup}
     */
    public AutologSetup autolog(){
        return new AutologSetup();
    }

    /**
     * Begins configuration of the Autometrics Autofeature.
     *
     * @return a fluent builder for {@link AutometricsSetup}
     */
    public AutometricsSetup autometrics(){
        return new AutometricsSetup();
    }

    /**
     * Begins configuration of the Autonotify Autofeature.
     *
     * @return a fluent builder for {@link AutonotifySetup}
     */
    public AutonotifySetup autonotify(){
        return new AutonotifySetup();
    }

    /**
     * Begins configuration of the Autoauth Autofeature.
     *
     * @return a fluent builder for {@link AutoauthSetup}
     */
    public AutoauthSetup autoauth(){
        return new AutoauthSetup();
    }

    /**
     * Begins configuration of the Shared Context Provider.
     *
     * @return a fluent builder for {@link SharedContextSetup}
     */
    public SharedContextSetup sharedContext(){
        return new SharedContextSetup();
    }

    public PropertiesSetup properties(){
        return new PropertiesSetup();
    }

    /**
     * Resets all configuration and stateful providers to their initial state.
     * <p>
     * Use this method carefully — it is primarily intended for tests or complete reconfiguration flows.
     */
    public void reset(){
        AutologProvider.SINGLETON.reset();
        AutometricsProvider.SINGLETON.reset();
        AutonotifyProvider.SINGLETON.reset();
        RoleRetrieverRegistry.SINGLETON.reset();
        Properties.SINGLETON.reset();
    }

    /**
     * Configuration options for the Autolog Autofeature.
     * <p>
     * Allows enabling/disabling async logging, I/O verbosity, and controlling stack trace details.
     */
    public static class AutologSetup{

        /**
         * Sets the logger instance to be used by the Autolog feature.
         * @param logger the logger implementation
         */
        public AutologSetup setProvidedInstance(Logger logger){
            AutologProvider.SINGLETON.setProvidedInstance(logger);
            return this;
        }

        /**
         * Enables or disables asynchronous logging.
         * @param async true for async logging
         */
        public AutologSetup setAsync(boolean async){
            AutologProvider.SINGLETON.setAsync(async);
            return this;
        }

        /**
         * Enables or disables logging input/output data for Use Cases.
         */
        public AutologSetup setUseCasesLoggingIO(boolean useCasesLoggingIO){
            AutologProvider.SINGLETON.setUseCasesLoggingIO(useCasesLoggingIO);
            return this;
        }

        /**
         * Enables or disables logging input/output data for Ports.
         */
        public AutologSetup setPortsLoggingIO(boolean portsLoggingIO){
            AutologProvider.SINGLETON.setPortsLoggingIO(portsLoggingIO);
            return this;
        }

        /**
         * Defines how I/O data is extracted (e.g., CAE_NATIVE or TO_STRING).
         */
        public AutologSetup setIOAutologMode(IOAutologMode ioAutologMode){
            AutologProvider.SINGLETON.setIOAutologMode(ioAutologMode);
            return this;
        }

        /**
         * Enables structured logging format or a simple-text format.
         */
        public AutologSetup setStructuredFormat(boolean structuredFormat){
            AutologProvider.SINGLETON.setStructuredFormat(structuredFormat);
            return this;
        }

        /**
         * Enables logging of stack traces for exceptions.
         */
        public AutologSetup setLoggingStackTrace(boolean loggingStackTrace){
            AutologProvider.SINGLETON.setLoggingStackTrace(loggingStackTrace);
            return this;
        }

        /**
         * Sets the number of stack trace lines to include when logging.
         */
        public AutologSetup setNumberOfLinesFromStackTrace(int numberOfLines){
            AutologProvider.SINGLETON.setNumberOfLinesFromStackTrace(numberOfLines);
            return this;
        }

        /**
         * Completes the autolog setup and returns to the main CAE setup chain.
         */
        public CaeSetup done(){
            if (AutologProvider.SINGLETON.getProvidedInstance().isEmpty())
                throw new InternalMappedException(
                        "CaeSetup misconfiguration on Autolog",
                        "Make sure you are providing a Logger's implementation instance"
                );
            return CaeSetup.SINGLETON;
        }

    }

    /**
     * Configuration options for the Autometrics Autofeature.
     * <p>
     * Autometrics captures latency, success/failure, and other metadata for Use Case and Port executions.
     */
    public static class AutometricsSetup{

        /**
         * Enables or disables asynchronous metric collection.
         */
        public AutometricsSetup setAsync(boolean async){
            AutometricsProvider.SINGLETON.setAsync(async);
            return this;
        }

        /**
         * Registers a custom subscriber to receive emitted metrics.
         */
        public AutometricsSetup subscribe(AutometricsSubscriber subscriber){
            AutometricsProvider.SINGLETON.subscribe(subscriber);
            return this;
        }

        /**
         * Completes the autometrics setup and returns to the main CAE setup chain.
         */
        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    /**
     * Configuration options for the Autonotify Autofeature.
     * <p>
     * Allows fine-grained control over when and how notifications are triggered based on exceptions or latency.
     */
    public static class AutonotifySetup{

        /**
         * Enables or disables asynchronous notification processing.
         */
        public AutonotifySetup setAsync(boolean async){
            AutonotifyProvider.SINGLETON.setAsync(async);
            return this;
        }

        /**
         * Registers a subscriber to receive notifications.
         */
        public AutonotifySetup subscribe(AutonotifySubscriber subscriber){
            AutonotifyProvider.SINGLETON.subscribe(subscriber);
            return this;
        }

        /**
         * Enables notification for InputMappedException instances being caught.
         */
        public AutonotifySetup considerInputMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerInputMappedExceptions();
            return this;
        }

        /**
         * Enables notification for NotFoundMappedException instances being caught.
         */
        public AutonotifySetup considerNotFoundMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotFoundMappedExceptions();
            return this;
        }

        /**
         * Enables notification for NotAuthenticatedMappedException instances being caught.
         */
        public AutonotifySetup considerNotAuthenticatedMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotAuthenticatedMappedExceptions();
            return this;
        }

        /**
         * Enables notification for NotAuthorizedMappedException instances being caught.
         */
        public AutonotifySetup considerNotAuthorizedMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotAuthorizedMappedExceptions();
            return this;
        }

        /**
         * Enables notification for InternalMappedException instances being caught.
         */
        public AutonotifySetup considerInternalMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerInternalMappedExceptions();
            return this;
        }

        /**
         * Enables notification for NoRetriesLeftException instances being caught.
         */
        public AutonotifySetup considerNoRetriesLeftExceptions(){
            AutonotifyProvider.SINGLETON.considerNoRetriesLeftExceptions();
            return this;
        }

        /**
         * Enables notification for MissingEnvVarException instances being caught.
         */
        public AutonotifySetup considerMissingEnvVarExceptions(){
            AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
            return this;
        }

        /**
         * Enables notification for all unexpected exceptions being caught (non MappedException).
         */
        public AutonotifySetup considerUnexpectedExceptions(){
            AutonotifyProvider.SINGLETON.considerUnexpectedExceptions();
            return this;
        }

        /**
         * Enables notification for a specific exception type if caught.
         */
        public <T extends Exception> AutonotifySetup considerSpecifically(Class<T> exceptionType){
            AutonotifyProvider.SINGLETON.considerSpecifically(exceptionType);
            return this;
        }

        /**
         * Enables latency-based notifications. Any execution exceeding the threshold (in milliseconds) will trigger a notification.
         */
        public AutonotifySetup considerLatency(Integer thresholdInMillis){
            AutonotifyProvider.SINGLETON.considerLatency(thresholdInMillis);
            return this;
        }

        /**
         * Completes the autonotify setup and returns to the main CAE setup chain.
         */
        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    /**
     * Configuration options for the Autoauth Autofeature.
     * <p>
     * Autoauth is responsible for dynamic authorization at the Use Case level.
     */
    public static class AutoauthSetup{

        /**
         * Sets the default role retriever to be used across all use cases.
         */
        public AutoauthSetup setDefaultRoleRetriever(RoleRetriever roleRetriever){
            RoleRetrieverRegistry.SINGLETON.setDefaultRoleRetriever(roleRetriever);
            return this;
        }

        /**
         * Overrides the default retriever for a specific Use Case ID.
         */
        public AutoauthSetup setRoleRetrieverByUseCaseId(RoleRetriever roleRetriever, String useCaseId){
            RoleRetrieverRegistry.SINGLETON.putRetrieverByUseCaseId(roleRetriever, useCaseId);
            return this;
        }

        /**
         * Completes the autoauth setup and returns to the main CAE setup chain.
         */
        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    /**
     * Configuration for setting shared objects across the application lifecycle.
     * <p>
     * Shared context values can be injected and reused within Use Cases or Autofeatures.
     */
    public static class SharedContextSetup{

        /**
         * Registers a shared object with a specific key.
         *
         * @param key          unique identifier
         * @param sharedObject the object to store
         */
        public SharedContextSetup put(String key, Object sharedObject){
            SharedContextProvider.SINGLETON.put(key, sharedObject);
            return this;
        }

        /**
         * Locks the context to prevent further mutation and returns to the main CAE setup chain.
         */
        public CaeSetup done(){
            SharedContextProvider.SINGLETON.lock();
            return CaeSetup.SINGLETON;
        }

    }

    public static class PropertiesSetup {

        public PropertyBuilder add(String name){
            return new PropertyBuilder(this, Property.of(name));
        }

        public PropertiesSetup add(String name, String singleValueProperty){
            Properties.SINGLETON.setProperty(Property.ofSingleValue(name, singleValueProperty));
            return this;
        }

        public PropertiesSetup add (Property property){
            Properties.SINGLETON.setProperty(property);
            return this;
        }

        public CaeSetup done(String activeProfile){
            Properties.SINGLETON.setActiveProfile(activeProfile);
            return CaeSetup.SINGLETON;
        }

        @RequiredArgsConstructor
        public static class PropertyBuilder{

            private final PropertiesSetup previousStep;
            private final Property property;

            public PropertyBuilder put(String profile, String value){
                this.property.setPair(profile, value);
                return this;
            }

            public PropertiesSetup wrapProperty(){
                Properties.SINGLETON.setProperty(this.property);
                return this.previousStep;
            }

        }

    }

}
