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
            this.generateLogExecutionStart();
            this.useCase.applyInternalLogic(this.useCaseExecutionCorrelation);
            this.generateLogExecutionEnd();
            this.logWhatsGeneratedForSuccessfulScenarios();
        } catch (Exception exception){
            this.handle(exception);
            this.logWhatsGeneratedForErrorScenarios();
            throw exception;
        }
    }
}
