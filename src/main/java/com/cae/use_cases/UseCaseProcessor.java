package com.cae.use_cases;


import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;

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

    protected void generateLogExecutionStart(){
        this.startingMoment = LocalDateTime.now();
        var messageToDisplay = "Use case \""
                + (this.useCase.getUseCaseMetadata().getName())
                + "\" execution with correlation ID of \""
                + this.useCaseExecutionCorrelation.getId().toString()
                + "\" ";
        this.stringBuilder.append(messageToDisplay);
    }

    protected void generateLogExecutionEnd(){
        var messageToDisplay = "finished successfully. It took about "
                + Duration.between(this.startingMoment, LocalDateTime.now()).toMillis()
                + " milliseconds.";
        this.stringBuilder.append(messageToDisplay);
    }

    protected void handle(Exception anyException){
        var messageToDisplay = "threw an exception. \""
                + (anyException.getClass().getSimpleName().concat(": ").concat(anyException.getMessage()))
                + ("\".");
        this.stringBuilder.append(messageToDisplay);
    }

    protected void logWhatsGeneratedForSuccessfulScenarios() {
        this.logger.logInfo(this.stringBuilder.toString());
    }

    protected void logWhatsGeneratedForErrorScenarios(){
        this.logger.logError(this.stringBuilder.toString());
    }

    protected <I extends UseCaseInput, O> void generateIOLog(I input, O output){
        if (Boolean.TRUE.equals(LoggerProvider.SINGLETON.getLogIO())){
            Optional.ofNullable(input).ifPresent(this::handleInputLogging);
            Optional.ofNullable(output).ifPresent(this::handleOutputLogging);
        }
    }

    private <I extends UseCaseInput> void handleInputLogging(I input) {
        this.stringBuilder.append(IOLoggingHandler.generateTextForLoggingInput(input, "USE CASE"));
    }

    private <O> void handleOutputLogging(O output) {
        this.stringBuilder.append(IOLoggingHandler.generateTextForLoggingOutput(output, "USE CASE"));
    }

}
