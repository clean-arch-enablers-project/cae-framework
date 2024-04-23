package com.cae.use_cases.specifics.functions;

import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;

public class FunctionUseCaseProcessor<I extends UseCaseInput, O> extends UseCaseProcessor<FunctionUseCase<I, O>> {

    public FunctionUseCaseProcessor(FunctionUseCase<I, O> useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public O processUseCaseUsing(I input){
        O output = null;
        try {
            this.generateLogExecutionStart();
            output = this.useCase.applyInternalLogic(input, this.useCaseExecutionCorrelation);
            this.generateLogExecutionEnd();
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(input, output);
            this.logWhatsGeneratedForSuccessfulScenarios();
            return output;
        } catch (Exception anyException){
            this.handle(anyException);
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(input, output);
            this.logWhatsGeneratedForErrorScenarios();
            throw anyException;
        }
    }

}
