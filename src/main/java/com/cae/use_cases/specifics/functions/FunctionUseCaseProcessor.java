package com.cae.use_cases.specifics.functions;

import com.cae.loggers.Logger;
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
            output = this.useCase.applyInternalLogic(input, this.useCaseExecutionCorrelation);
            this.generateLogForSuccessfulExecution(input, output);
            return output;
        } catch (Exception anyException){
            this.generateLogForUnsuccessfulExecution(anyException, input, output);
            throw anyException;
        }
    }

}
