package com.cae.use_cases;

import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.metadata.UseCaseMetadata;

/**
 * Abstract type of UseCase. A UseCase will have an instance of UseCaseMetadata
 * and Logger. So, any UseCase instance will be able to internally access its
 * metadata as well as interact with the logger implementation injected.
 */
public abstract class UseCase {

    /**
     * Metadata such as UseCase name, description and protection status
     */
    protected final UseCaseMetadata useCaseMetadata;

    /**
     * Logger implementation for free use.
     */
    private final Logger logger;

    protected UseCase(UseCaseMetadata useCaseMetadata, Logger logger) {
        this.useCaseMetadata = useCaseMetadata;
        this.logger = logger;
    }

    protected UseCase(Logger logger) {
        this.useCaseMetadata = UseCaseMetadata.ofOpenAccessUseCase(this.getClass());
        this.logger = logger;
    }

    protected UseCase() {
        this.useCaseMetadata = UseCaseMetadata.ofOpenAccessUseCase(this.getClass());
        this.logger = null;
    }

    public UseCaseMetadata getUseCaseMetadata(){
        return this.useCaseMetadata;
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

}
