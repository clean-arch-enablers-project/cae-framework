package com.cae.use_cases.specifics.suppliers;

import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.UseCaseProcessor;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

public class SupplierUseCaseProcessor<O> extends UseCaseProcessor<SupplierUseCase<O>> {

    public SupplierUseCaseProcessor(SupplierUseCase<O> useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        super(useCase, useCaseExecutionCorrelation, logger);
    }

    public O processUseCase(){
        O output = null;
        try {
            this.generateLogExecutionStart();
            output = this.useCase.applyInternalLogic(this.useCaseExecutionCorrelation);
            this.generateLogExecutionEnd();
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(null, output);
            this.logWhatsGeneratedForSuccessfulScenarios();
            return output;
        } catch (Exception anyException){
            this.handle(anyException);
            if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent())
                this.generateIOLog(null, output);
            this.logWhatsGeneratedForErrorScenarios();
            throw anyException;
        }
    }

}
