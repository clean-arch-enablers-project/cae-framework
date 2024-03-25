package com.cae.use_cases;


import com.cae.loggers.Logger;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

import java.time.Duration;
import java.time.LocalDateTime;

public abstract class UseCaseProcessor<U extends UseCase> {

    protected UseCaseProcessor(U useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger){
        this.useCase = useCase;
        this.useCaseExecutionCorrelation = useCaseExecutionCorrelation;
        this.logger = logger;
    }
    protected final Logger logger;
    protected final U useCase;
    protected final UseCaseExecutionCorrelation useCaseExecutionCorrelation;
    private final StringBuilder stringBuilder = new StringBuilder();
    private LocalDateTime startingMoment;

    protected void logExecutionStart(){
        this.startingMoment = LocalDateTime.now();
        var messageToDisplay = "Use case \""
                + (this.useCase.getUseCaseMetadata().getName())
                + "\" execution with correlation ID of \""
                + this.useCaseExecutionCorrelation.getId().toString()
                + "\" ";
        this.stringBuilder.append(messageToDisplay);
    }

    protected void logExecutionEnd(){
        var messageToDisplay = "finished successfully. It took about "
                + Duration.between(this.startingMoment, LocalDateTime.now()).toMillis()
                + " milliseconds.";
        this.stringBuilder.append(messageToDisplay);
        this.logger.logInfo(this.stringBuilder.toString());
    }

    protected void handle(Exception anyException){
        var messageToDisplay = "threw an exception. \""
                + (anyException.getClass().getSimpleName().concat(": ").concat(anyException.getMessage()))
                + ("\".");
        this.stringBuilder.append(messageToDisplay);
        this.logger.logError(this.stringBuilder.toString());
    }

}
