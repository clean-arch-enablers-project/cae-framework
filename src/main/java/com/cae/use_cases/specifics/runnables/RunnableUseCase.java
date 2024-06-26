package com.cae.use_cases.specifics.runnables;

import com.cae.loggers.Logger;
import com.cae.trier.Trier;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseProcessorFactory;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.metadata.UseCaseMetadata;

/**
 * Specific type of UseCase which has neither input nor output
 */
public abstract class RunnableUseCase extends UseCase {

    /**
     *  @deprecated
     */
    @Deprecated(since = "24/04/2024")
    protected RunnableUseCase(UseCaseMetadata useCaseMetadata, Logger logger) {
        super(useCaseMetadata, logger);
    }
    /**
     *  @deprecated
     */
    @Deprecated(since = "24/04/2024")
    protected RunnableUseCase(Logger logger) {
        super(logger);
    }
    protected RunnableUseCase() {
        super();
    }

    /**
     * Public method which triggers the execution of the RunnableUseCase.
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
     * @param correlation the unique identifier of the use case execution
     */
    public void execute(UseCaseExecutionCorrelation correlation){
        this.handleAuthorization(correlation);
        Trier.of(() -> UseCaseProcessorFactory.of(this, correlation, this.getLogger()).processUseCase())
                .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
                .finishAndExecuteAction();
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param useCaseExecutionCorrelation the unique identifier of the use case execution
     */
    protected abstract void applyInternalLogic(UseCaseExecutionCorrelation useCaseExecutionCorrelation);

}
