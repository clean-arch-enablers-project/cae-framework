package com.cae.use_cases.specifics.consumers;

import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;

public class ConsumerUseCaseProcessor<I extends UseCaseInput> extends UseCaseProcessor<ConsumerUseCase<I>> {

    public ConsumerUseCaseProcessor(ConsumerUseCase<I> useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public void processUseCaseUsing(I input){
        try {
            this.generateLogExecutionStart();
            this.useCase.applyInternalLogic(input, this.useCaseExecutionCorrelation);
            this.generateLogExecutionEnd();
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(input, null);
            this.logWhatsGeneratedForSuccessfulScenarios();
        } catch (Exception anyException){
            this.handle(anyException);
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(input, null);
            this.logWhatsGeneratedForErrorScenarios();
            throw anyException;
        }
    }
}
