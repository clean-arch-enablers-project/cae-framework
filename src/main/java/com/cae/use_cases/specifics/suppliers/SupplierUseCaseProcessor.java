package com.cae.use_cases.specifics.suppliers;

import com.cae.loggers.Logger;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

public class SupplierUseCaseProcessor<O> extends UseCaseProcessor<SupplierUseCase<O>> {

    public SupplierUseCaseProcessor(SupplierUseCase<O> useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public O processUseCase(){
        try {
            this.logExecutionStart();
            var output = this.useCase.applyInternalLogic(this.useCaseExecutionCorrelation);
            this.logExecutionEnd();
            return output;
        } catch (Exception anyException){
            this.handle(anyException);
            throw anyException;
        }
    }

}
