package com.cae.use_cases;


import com.cae.loggers.Logger;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.specifics.consumers.ConsumerUseCase;
import com.cae.use_cases.specifics.consumers.ConsumerUseCaseProcessor;
import com.cae.use_cases.specifics.functions.FunctionUseCase;
import com.cae.use_cases.specifics.functions.FunctionUseCaseProcessor;
import com.cae.use_cases.specifics.runnables.RunnableUseCase;
import com.cae.use_cases.specifics.runnables.RunnableUseCaseProcessor;
import com.cae.use_cases.specifics.suppliers.SupplierUseCase;
import com.cae.use_cases.specifics.suppliers.SupplierUseCaseProcessor;

public class UseCaseProcessorFactory {

    private UseCaseProcessorFactory(){}

    public static <I extends UseCaseInput, O> FunctionUseCaseProcessor<I, O> of(
            FunctionUseCase<I, O> useCase,
            UseCaseExecutionCorrelation useCaseExecutionCorrelation,
            Logger logger){
        return new FunctionUseCaseProcessor<>(useCase, useCaseExecutionCorrelation, logger);
    }

    public static <I extends UseCaseInput> ConsumerUseCaseProcessor<I> of(
            ConsumerUseCase<I> useCase,
            UseCaseExecutionCorrelation useCaseExecutionCorrelation,
            Logger logger){
        return new ConsumerUseCaseProcessor<>(useCase, useCaseExecutionCorrelation, logger);
    }

    public static <O> SupplierUseCaseProcessor<O> of(
            SupplierUseCase<O> useCase,
            UseCaseExecutionCorrelation useCaseExecutionCorrelation,
            Logger logger){
        return new SupplierUseCaseProcessor<>(useCase, useCaseExecutionCorrelation, logger);
    }

    public static RunnableUseCaseProcessor of(
            RunnableUseCase useCase,
            UseCaseExecutionCorrelation useCaseExecutionCorrelation,
            Logger logger){
        return new RunnableUseCaseProcessor(useCase, useCaseExecutionCorrelation, logger);
    }
}
