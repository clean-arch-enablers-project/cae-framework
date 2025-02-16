package com.cae.autolog;

import com.cae.autolog.formats.IO;
import com.cae.autolog.formats.UseCaseLogStructuredFormat;
import com.cae.autolog.native_io_extraction_mode.NativeExtractionMode;
import com.cae.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.ports.autolog.PortInsights;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class Autolog<U extends UseCase> {

    private final U useCase;
    private final StringBuilder stringBuilder;
    private final Logger logger;
    private final ExecutionContext correlation;

    public static <U extends UseCase> Autolog<U> of(
            U useCase,
            ExecutionContext correlation){
        return new Autolog<>(
                useCase,
                new StringBuilder(),
                useCase.getLogger(),
                correlation);
    }

    public void logExecution(
            ExecutionContext context,
            UseCaseInput input,
            Object output,
            Exception exception,
            Long latency){
        this.synchronousExecutionIfNeeded(CompletableFuture.runAsync(() -> {
            if (Boolean.TRUE.equals(AutologProvider.SINGLETON.getStructuredFormat()))
                this.generateLogInStructuredFormat(input, output, exception, context, latency);
            else
                this.generateLogInSimpleFormat(input, output, exception, context, latency);
            PortInsights.SINGLETON.flush(context);
            this.logWhatsGenerated(exception == null);
        }));
    }

    private void synchronousExecutionIfNeeded(CompletableFuture<Void> future) {
        if (Boolean.FALSE.equals(AutologProvider.SINGLETON.getAsync()))
            future.join();
    }

    private void generateLogInSimpleFormat(
            UseCaseInput input,
            Object output,
            Exception exception,
            ExecutionContext context,
            Long latency){
        if (exception == null)
            this.generateSimpleLogForSuccessfulExecution(input, output, context, latency);
        else
            this.generateSimpleLogForUnsuccessfulExecution(exception, input, output, context, latency);
    }

    private void generateSimpleLogForSuccessfulExecution(
            UseCaseInput input,
            Object output,
            ExecutionContext context,
            Long latency){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfSuccessfulLogString(latency))
                .append(Optional.ofNullable(PortInsights.SINGLETON.getFor(context)).map(insights -> " | Port insights: " + insights).orElse(""));
        this.generateIOLogString(input, output);
    }

    private void generateSimpleLogForUnsuccessfulExecution(
            Exception anyException,
            UseCaseInput input,
            Object output,
            ExecutionContext context,
            Long latency){
        this.stringBuilder
                .append(this.generateFirstHalfOfLogString())
                .append(this.generateLastHalfOfUnsuccessfulLogString(anyException, latency))
                .append(Optional.ofNullable(PortInsights.SINGLETON.getFor(context)).map(insights -> " | Port insights: " + SimpleJsonBuilder.buildFor(insights)).orElse(""));
        this.generateIOLogString(input, output);
    }

    private <I extends UseCaseInput, O> void generateIOLogString(I input, O output){
        if (AutologProvider.SINGLETON.getProvidedInstance().isPresent() && Boolean.TRUE.equals(AutologProvider.SINGLETON.getUseCasesLoggingIO())){
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
        return "Use case '"
                + (this.useCase.getUseCaseMetadata().getName())
                + "' execution with correlation ID of '"
                + this.correlation.getCorrelationId().toString();
    }

    private String generateLastHalfOfSuccessfulLogString(Long latency){
        return "' finished successfully. It took about "
                + latency
                + " milliseconds.";
    }

    private String generateLastHalfOfUnsuccessfulLogString(Exception anyException, Long latency){
        return "' threw an exception and took about "
                + latency
                + " milliseconds. '"
                + (anyException.getClass().getSimpleName().concat(": ").concat(anyException.getMessage()))
                + ("'.");
    }

    private void generateLogInStructuredFormat(
            UseCaseInput input,
            Object output,
            Exception exception,
            ExecutionContext context,
            Long latency) {
        var io = Boolean.FALSE.equals(AutologProvider.SINGLETON.getUseCasesLoggingIO())? null : IO.builder()
                .input(input)
                .output(output)
                .build();
        var executionData = UseCaseLogStructuredFormat.UseCaseExecutionLogFormat.builder()
                .useCase(this.useCase.getUseCaseMetadata().getName())
                .correlationId(this.correlation.toString())
                .latency(latency)
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
