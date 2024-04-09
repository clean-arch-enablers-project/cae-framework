package com.cae.use_cases.specifics.consumers;

import com.cae.loggers.Logger;
import com.cae.trier.Trier;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseProcessorFactory;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.metadata.UseCaseMetadata;

/**
 * Specific type of UseCase which only has input but no output.
 * @param <I> type of input
 */
public abstract class ConsumerUseCase <I extends UseCaseInput> extends UseCase {

    protected ConsumerUseCase(UseCaseMetadata useCaseMetadata, Logger logger) {
        super(useCaseMetadata, logger);
    }

    /**
     * Public method which triggers the execution of the ConsumerUseCase.
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
     */
    public void execute(I input, UseCaseExecutionCorrelation correlation){
        input.validateProperties();
        Trier.of(() ->  UseCaseProcessorFactory.of(this, correlation, this.logger).processUseCaseUsing(input))
                .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
                .finishAndExecuteAction();
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param input input of the use case
     */
    protected abstract void applyInternalLogic(I input, UseCaseExecutionCorrelation correlation);


}
