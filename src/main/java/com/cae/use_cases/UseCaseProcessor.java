package com.cae.use_cases;


import com.cae.loggers.Logger;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public abstract class UseCaseProcessor<U extends UseCase> {

    protected final U useCase;
    protected final UseCaseExecutionCorrelation correlation;
    protected final Logger logger;

    public void logExecution(UseCaseInput input, Object output, Exception exception) {
        UseCaseLoggingManagement.of(this.useCase, this.logger, this.correlation).handleUseCaseExecutionLog(input, output, exception);
    }

}
