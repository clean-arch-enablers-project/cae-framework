package com.cae.use_cases;

import com.cae.autofeatures.autolog.Logger;
import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.autofeatures.autoauth.ScopeBasedAuth;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.autofeatures.autoauth.exceptions.NotAllowedMappedException;
import com.cae.use_cases.metadata.UseCaseMetadata;
import com.cae.use_cases.registries.UseCaseRegistry;
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

    protected void handleScopeBasedAuthorization(ExecutionContext executionContext){
        if (Boolean.TRUE.equals(this.useCaseMetadata.isProtected()) && Boolean.TRUE.equals(this.useCaseMetadata.getScope().length > 0)){
            var actor = executionContext.getActor()
                    .orElseThrow(() -> new InternalMappedException(
                            "No actor instance provided",
                            "For executing protected use cases, you must provide an instance of Actor via the ExecutionContext object. Please fix it and try again."
                    ));
            if (!ScopeBasedAuth.allows(actor, this.useCaseMetadata.getScope())) throw new NotAllowedMappedException(this);
        }
    }

}
