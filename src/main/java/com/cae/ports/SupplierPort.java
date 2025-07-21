package com.cae.ports;


import com.cae.autofeatures.autocache.Cache;
import com.cae.autofeatures.autocache.Cacheable;
import com.cae.autofeatures.autocache.DefaultCaeAutocache;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: supplier ports are ports that don't have input
 * but supply something as return of their execution.
 * @param <O> the output type
 */
public abstract class SupplierPort <O> extends Port implements Cacheable {


    protected SupplierPort() {
        super();
        this.cache = this.usesAutocache()? new DefaultCaeAutocache<>(this.name, this.getAutocacheMetadata()) : null;
    }

    protected final Cache<O> cache;

    public O executePort(ExecutionContext context){
        return Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            try {
                var output = this.getOutputFor(context);
                stepInsights.complete();
                stepInsights.setOutput(output);
                return output;
            } catch (Exception anyException){
                stepInsights.complete(anyException);
                throw anyException;
            }
        })
        .onUnexpectedExceptions(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    private O getOutputFor(ExecutionContext context) {
        if (this.usesAutocache()){
            var autocacheKey = this.getName();
            return this.cache.get(autocacheKey, context).orElseGet(() -> {
                var output = this.executeLogic(context);
                this.cache.put(autocacheKey, output, context);
                return output;
            });
        }
        return this.executeLogic(context);
    }

    protected abstract O executeLogic(ExecutionContext correlation);

}
