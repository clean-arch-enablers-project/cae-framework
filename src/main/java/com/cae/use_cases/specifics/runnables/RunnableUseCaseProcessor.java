package com.cae.use_cases.specifics.runnables;

import com.cae.loggers.Logger;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

public class RunnableUseCaseProcessor extends UseCaseProcessor<RunnableUseCase> {

    public RunnableUseCaseProcessor(RunnableUseCase useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public void processUseCase() {
        try {
            this.useCase.applyInternalLogic(this.useCaseExecutionCorrelation);
            this.generateLogForSuccessfulExecution(null, null);
        } catch (Exception exception){
            this.generateLogForUnsuccessfulExecution(exception, null, null);
            throw exception;
        }
    }
}
