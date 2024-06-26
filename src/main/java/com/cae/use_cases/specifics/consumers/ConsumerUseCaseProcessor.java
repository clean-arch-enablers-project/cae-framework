package com.cae.use_cases.specifics.consumers;

import com.cae.loggers.Logger;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;

public class ConsumerUseCaseProcessor<I extends UseCaseInput> extends UseCaseProcessor<ConsumerUseCase<I>> {

    public ConsumerUseCaseProcessor(ConsumerUseCase<I> useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public void processUseCaseUsing(I input){
        try {
            this.useCase.applyInternalLogic(input, this.correlation);
            this.logExecution(input, null, null);
        } catch (Exception anyException){
            this.logExecution(input, null, anyException);
            throw anyException;
        }
    }
}
