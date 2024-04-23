package com.cae.use_cases.specifics.functions;

import com.cae.loggers.Logger;
import com.cae.trier.Trier;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseProcessorFactory;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.metadata.UseCaseMetadata;

/**
 * Specific type of UseCase which has both input and output
 * @param <I> type of input
 * @param <O> type of output
 */
public abstract class FunctionUseCase <I extends UseCaseInput, O> extends UseCase {

    /**
     *  @deprecated
     */
    @Deprecated(since = "24/04/2024")
    protected FunctionUseCase(UseCaseMetadata useCaseMetadata, Logger logger) {
        super(useCaseMetadata, logger);
    }
    /**
     *  @deprecated
     */
    @Deprecated(since = "24/04/2024")
    protected FunctionUseCase(Logger logger) {
        super(logger);
    }
    protected FunctionUseCase() {
        super();
    }

    /**
     * Public method which triggers the execution of the FunctionUseCase.
     * It will internally call the method which keeps the core logic of the
     * use case. If anything goes unexpectedly wrong during its execution,
     * it will throw a UseCaseExecutionException instance with the description
     * of what have gone wrong. If your use case implementation throws
     * a MappedException instance, it will not intercede as it will consider
     * the MappedException as part of the planned flow.
     * Executing your use case with this method assures there will be
     * automated log tracking control: the beginning and the ending of the
     * use case execution will be logged, weather it ends successfully or not.
     * However, you are still free to use your logger instance as you wish
     * inside your use case implementations.
     * @param input the input of the use case
     * @return the output of the use case
     */
    public O execute(I input, UseCaseExecutionCorrelation correlation){
        input.validateProperties();
        return Trier.of(() ->  UseCaseProcessorFactory.of(this, correlation, this.getLogger()).processUseCaseUsing(input))
                    .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
                .finishAndExecuteAction();
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param input input of the use case
     * @return output of the use case
     */
    protected abstract O applyInternalLogic(I input, UseCaseExecutionCorrelation correlation);

}
