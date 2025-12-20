package com.cae.framework.use_cases;

import com.cae.framework.autofeatures.autolog.AutologProvider;
import com.cae.framework.autofeatures.autolog.Logger;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.framework.use_cases.metadata.UseCaseMetadata;
import com.cae.framework.use_cases.registries.UseCaseRegistry;
import lombok.Getter;

/**
 * Abstract type of UseCase. A UseCase will have an instance of UseCaseMetadata
 * and Logger. So, any UseCase instance will be able to internally access its
 * metadata as well as interact with the logger implementation injected.
 */
@Getter
public abstract class UseCase {

    /**
     * Metadata such as UseCase name, description and protection status
     */
    protected final UseCaseMetadata useCaseMetadata;

    protected UseCase() {
        this.useCaseMetadata = UseCaseMetadata.of(this);
        UseCaseRegistry.SINGLETON.add(this);
    }

    public Logger getLogger(){
        return AutologProvider.SINGLETON.getProvidedInstance()
                .orElseThrow(this::handleNoLoggerProvided);
    }

    private MappedException handleNoLoggerProvided() {
        return new InternalMappedException(
                "No logger instance provided for the use case \"" + this.getUseCaseMetadata().getName() + "\"",
                "Please provide an instance via the AutologProvider singleton instance"
        );
    }

}
