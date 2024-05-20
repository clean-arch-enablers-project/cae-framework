package com.cae.use_cases;

import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.authorization.UseCaseExecutionAuthorizer;
import com.cae.use_cases.authorization.annotations.ProtectedUseCase;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.NotAllowedMappedException;
import com.cae.use_cases.metadata.UseCaseMetadata;
import lombok.Getter;

import java.util.Optional;

/**
 * Abstract type of UseCase. A UseCase will have an instance of UseCaseMetadata
 * and Logger. So, any UseCase instance will be able to internally access its
 * metadata as well as interact with the logger implementation injected.
 */
public abstract class UseCase {

    /**
     * Metadata such as UseCase name, description and protection status
     */
    @Getter
    protected final UseCaseMetadata useCaseMetadata;

    /**
     * Logger implementation for free use.
     */
    private final Logger logger;

    protected UseCase(UseCaseMetadata useCaseMetadata, Logger logger) {
        this.useCaseMetadata = useCaseMetadata;
        this.logger = logger;
        UseCaseRegistry.SINGLETON.add(this);
    }

    protected UseCase(Logger logger) {
        this.useCaseMetadata = UseCase.extractSomeMetadataFrom(this.getClass());
        this.logger = logger;
        UseCaseRegistry.SINGLETON.add(this);
    }

    protected UseCase() {
        this.useCaseMetadata = UseCase.extractSomeMetadataFrom(this.getClass());
        this.logger = null;
        UseCaseRegistry.SINGLETON.add(this);
    }

    private static UseCaseMetadata extractSomeMetadataFrom(Class<? extends UseCase> thisType) {
        var theType = UseCase.findOutWhichClassIsAnnotated(thisType);
        return theType.map(aClass -> UseCaseMetadata.ofProtectedUseCase(thisType, extractScopesFrom(aClass)))
                .orElseGet(() -> UseCaseMetadata.ofOpenAccessUseCase(thisType));
    }

    private static Optional<Class<?>> findOutWhichClassIsAnnotated(Class<?> thisType) {
        var isProtected = thisType.isAnnotationPresent(ProtectedUseCase.class);
        if (isProtected)
            return Optional.of(thisType);
        if (thisType == UseCase.class)
            return Optional.empty();
        return UseCase.findOutWhichClassIsAnnotated(thisType.getSuperclass());
    }

    private static String[] extractScopesFrom(Class<?> thisType) {
        return thisType.getAnnotation(ProtectedUseCase.class).scope();
    }

    protected Logger getLogger(){
        if (this.logger != null)
            return this.logger;
        return LoggerProvider.SINGLETON.getProvidedInstance()
                .orElseThrow(this::handleNoLoggerProvided);
    }

    private MappedException handleNoLoggerProvided() {
        return new InternalMappedException("No logger instance provided for the use case \"" + this.getUseCaseMetadata().getName() + "\"", "Either provide an instance via the use case constructor or via the LoggerProvider singleton instance");
    }

    protected void handleAuthorization(UseCaseExecutionCorrelation useCaseExecutionCorrelation){
        if (Boolean.TRUE.equals(this.useCaseMetadata.isProtected())){
            var actor = useCaseExecutionCorrelation.getActor()
                    .orElseThrow(() -> new InternalMappedException("No actor instance provided", "For executing protected use cases, you must provide an instance of Actor via the UseCaseExecutionCorrelation object. Fix it and try again."));
            if (!UseCaseExecutionAuthorizer.allows(actor, this.useCaseMetadata.getScope())) {
                throw new NotAllowedMappedException(this);
            }
        }
    }

}
