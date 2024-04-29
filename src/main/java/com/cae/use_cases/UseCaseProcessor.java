package com.cae.use_cases;


import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.loggers.formats.IO;
import com.cae.loggers.formats.UseCaseLogStructuredFormat;
import com.cae.loggers.native_io_extraction_mode.NativeExtractionMode;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.io.UseCaseInput;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

public abstract class UseCaseProcessor<U extends UseCase> {

    protected final U useCase;
    protected final UseCaseExecutionCorrelation useCaseExecutionCorrelation;
    protected final Logger logger;
    private final StringBuilder stringBuilder;
    private final LocalDateTime startingMoment;

    protected UseCaseProcessor(U useCase, UseCaseExecutionCorrelation useCaseExecutionCorrelation, Logger logger) {
        this.useCase = useCase;
        this.useCaseExecutionCorrelation = useCaseExecutionCorrelation;
        this.logger = logger;
        this.stringBuilder = new StringBuilder();
        this.startingMoment = LocalDateTime.now();
    }

    public void logExecution(UseCaseInput input, Object output, Exception exception){
        this.synchronousExecutionIfNeeded(CompletableFuture.runAsync(() -> {
            if (Boolean.TRUE.equals(LoggerProvider.SINGLETON.getStructuredFormat()))
                this.generateLogInStructuredFormat(input, output, exception);
            else
                this.generateLogInSimpleFormat(input, output, exception);
            this.logWhatsGenerated(exception == null);
        }));
    }

    private void synchronousExecutionIfNeeded(CompletableFuture<Void> future) {
        if (Boolean.FALSE.equals(LoggerProvider.SINGLETON.getAsync()))
            future.join();
    }

    private void generateLogInSimpleFormat(
            UseCaseInput input,
            Object output,
            Exception exception){
        if (exception == null)
            this.generateLogForSuccessfulExecution(input, output);
        else
            this.generateLogForUnsuccessfulExecution(exception, input, output);
    }

    protected void generateLogForSuccessfulExecution(
            UseCaseInput input,
            Object output){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfSuccessfulLogString());
        this.generateIOLogString(input, output);
    }

    protected void generateLogForUnsuccessfulExecution(
            Exception anyException,
            UseCaseInput input,
            Object output){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfUnsuccessfulLogString(anyException));
        this.generateIOLogString(input, output);
    }

    protected <I extends UseCaseInput, O> void generateIOLogString(I input, O output){
        if (LoggerProvider.SINGLETON.getProvidedInstance().isPresent() && Boolean.TRUE.equals(LoggerProvider.SINGLETON.getUseCasesLoggingIO())){
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

    private String generateFirstHalfOfLogString(){
        return "Use case \""
                + (this.useCase.getUseCaseMetadata().getName())
                + "\" execution with correlation ID of \""
                + this.useCaseExecutionCorrelation.getId().toString();
    }

    private String generateLastHalfOfSuccessfulLogString(){
        return "\" finished successfully. It took about "
                + Duration.between(this.startingMoment, LocalDateTime.now()).toMillis()
                + " milliseconds.";
    }

    private String generateLastHalfOfUnsuccessfulLogString(Exception anyException){
        return "\" threw an exception. \""
                + (anyException.getClass().getSimpleName().concat(": ").concat(anyException.getMessage()))
                + ("\".");
    }

    private void generateLogInStructuredFormat(
            UseCaseInput input,
            Object output,
            Exception exception) {
        var io = Boolean.FALSE.equals(LoggerProvider.SINGLETON.getUseCasesLoggingIO())? null : IO.builder()
                .input(input)
                .output(output)
                .build();
        var executionData = UseCaseLogStructuredFormat.UseCaseExecutionLogFormat.builder()
                .useCase(this.useCase.getUseCaseMetadata().getName())
                .correlationId(this.useCaseExecutionCorrelation.toString())
                .latency(Duration.between(this.startingMoment, LocalDateTime.now()).toMillis())
                .successful(exception == null)
                .exception(exception == null? null : exception.toString())
                .io(io)
                .build();
        var structuredFormat = UseCaseLogStructuredFormat.builder()
                .useCaseExecution(executionData)
                .build();
        this.stringBuilder.append(NativeExtractionMode.executeOn(structuredFormat));
    }

    private void logWhatsGenerated(boolean success) {
        Consumer<String> methodToCall = (success? this.logger::logInfo : this.logger::logError);
        methodToCall.accept(this.stringBuilder.toString());
    }

}
