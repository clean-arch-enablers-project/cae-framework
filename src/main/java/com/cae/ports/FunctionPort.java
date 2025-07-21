package com.cae.ports;

import com.cae.autofeatures.autocache.AutocacheKeyExtractor;
import com.cae.autofeatures.autocache.Cache;
import com.cae.autofeatures.autocache.Cacheable;
import com.cae.autofeatures.autocache.DefaultCaeAutocache;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: function ports are ports that both receive
 * inputs and return outputs.
 * @param <I> the input type
 * @param <O> the output type
 */
public abstract class FunctionPort <I, O> extends Port implements Cacheable {

    protected FunctionPort(){
        super();
        this.cache = this.usesAutocache()? new DefaultCaeAutocache<>(this.name, this.getAutocacheMetadata()) : null;
    }

    protected final Cache<O> cache;

    public O executePortOn(I input, ExecutionContext context){
        return Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            stepInsights.setInput(input);
            try {
                var output = this.getOutputFor(input, context);
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

    private O getOutputFor(I input, ExecutionContext context) {
        if (this.usesAutocache()){
            var autocacheKey = AutocacheKeyExtractor.runOn(input);
            return this.cache.get(autocacheKey, context).orElseGet(() -> {
                var output = this.executeLogic(input, context);
                this.cache.put(autocacheKey, output, context);
                return output;
            });
        }
        return this.executeLogic(input, context);
    }

    protected abstract O executeLogic(I input, ExecutionContext correlation);

}
