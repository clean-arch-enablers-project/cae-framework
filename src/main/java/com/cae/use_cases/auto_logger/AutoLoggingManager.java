package com.cae.use_cases.auto_logger;

import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.Logger;
import com.cae.loggers.LoggerProvider;
import com.cae.loggers.formats.IO;
import com.cae.loggers.formats.UseCaseLogStructuredFormat;
import com.cae.loggers.native_io_extraction_mode.NativeExtractionMode;
import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.ports.auto_logging.PortInsights;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class AutoLoggingManager<U extends UseCase> {

    private final U useCase;
    private final StringBuilder stringBuilder;
    private final LocalDateTime startingMoment;
    private final Logger logger;
    private final ExecutionContext correlation;

    public static <U extends UseCase> AutoLoggingManager<U> of(
            U useCase,
            ExecutionContext correlation){
        return new AutoLoggingManager<>(
                useCase,
                new StringBuilder(),
                LocalDateTime.now(),
                useCase.getLogger(),
                correlation);
    }

    public void logExecution(
            ExecutionContext context,
            UseCaseInput input,
            Object output,
            Exception exception){
        this.synchronousExecutionIfNeeded(CompletableFuture.runAsync(() -> {
            if (Boolean.TRUE.equals(LoggerProvider.SINGLETON.getStructuredFormat()))
                this.generateLogInStructuredFormat(input, output, exception, context);
            else
                this.generateLogInSimpleFormat(input, output, exception, context);
            PortInsights.SINGLETON.flush(context);
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
            Exception exception,
            ExecutionContext context){
        if (exception == null)
            this.generateSimpleLogForSuccessfulExecution(input, output, context);
        else
            this.generateSimpleLogForUnsuccessfulExecution(exception, input, output, context);
    }

    private void generateSimpleLogForSuccessfulExecution(
            UseCaseInput input,
            Object output,
            ExecutionContext context){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfSuccessfulLogString())
                .append(Optional.ofNullable(PortInsights.SINGLETON.getFor(context)).map(insights -> " | Port insights: " + insights).orElse(""));
        this.generateIOLogString(input, output);
    }

    private void generateSimpleLogForUnsuccessfulExecution(
            Exception anyException,
            UseCaseInput input,
            Object output,
            ExecutionContext context){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfUnsuccessfulLogString(anyException))
                .append(Optional.ofNullable(PortInsights.SINGLETON.getFor(context)).map(insights -> " | Port insights: " + SimpleJsonBuilder.buildFor(insights)).orElse(""));
        this.generateIOLogString(input, output);
    }

    private <I extends UseCaseInput, O> void generateIOLogString(I input, O output){
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
                + this.correlation.getCorrelationId().toString();
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
            Exception exception, ExecutionContext context) {
        var io = Boolean.FALSE.equals(LoggerProvider.SINGLETON.getUseCasesLoggingIO())? null : IO.builder()
                .input(input)
                .output(output)
                .build();
        var executionData = UseCaseLogStructuredFormat.UseCaseExecutionLogFormat.builder()
                .useCase(this.useCase.getUseCaseMetadata().getName())
                .correlationId(this.correlation.toString())
                .latency(Duration.between(this.startingMoment, LocalDateTime.now()).toMillis())
                .successful(exception == null)
                .exception(exception == null? null : exception.toString())
                .io(io)
                .portInsights(Optional.ofNullable(PortInsights.SINGLETON.getFor(context)).orElse(new ArrayList<>()).stream().map(insight -> insight.replace("\"", "\\\"")).collect(Collectors.toList()))
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
