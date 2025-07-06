package com.cae.autofeatures.autolog;

import com.cae.autofeatures.autolog.formats.IO;
import com.cae.autofeatures.autolog.formats.UseCaseLogStructuredFormat;
import com.cae.autofeatures.autolog.native_io_extraction_mode.NativeExtractionMode;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.ExecutionContext;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Autolog {

    private Autolog(){}

    public static void runOn(ExecutionContext executionContext) {
        AutologThreadPoolProvider.SINGLETON.getExecutor()
                .submit(() -> {
                    Autolog.handleStackTraceOn(executionContext);
                    Autolog.writeBasedOn(executionContext);
                });
    }

    private static void handleStackTraceOn(ExecutionContext executionContext) {
        if (!executionContext.wasSuccessful()) StackTraceAutologAux.SINGLETON.handleLoggingStackTrace(executionContext);
    }

    private static void writeBasedOn(ExecutionContext executionContext) {
        boolean isStructuredFormat = AutologProvider.SINGLETON.getStructuredFormat();
        var logger = Autolog.getProvidedLogger();
        Consumer<String> loggerAction = executionContext.wasSuccessful()? logger::logInfo : logger::logError;
        loggerAction.accept(isStructuredFormat? Autolog.generateStructuredFormat(executionContext) : Autolog.generateSimpleTextFormat(executionContext));
    }

    private static Logger getProvidedLogger() {
        return AutologProvider.SINGLETON.getProvidedInstance().orElseThrow(() -> new InternalMappedException(
                "Couldn't execute Autolog",
                "The framework does not provide a default Logger implementation, you must supply one yourself."
        ));
    }

    private static String generateStructuredFormat(ExecutionContext executionContext) {
        var io = Boolean.FALSE.equals(AutologProvider.SINGLETON.getUseCasesLoggingIO())? null : IO.builder()
                .input(executionContext.getInput())
                .output(executionContext.getOutput())
                .build();
        var executionData = UseCaseLogStructuredFormat.UseCaseExecutionLogFormat.builder()
                .useCase(executionContext.getSubject())
                .correlationId(executionContext.toString())
                .latency(executionContext.calculateLatency())
                .successful(executionContext.wasSuccessful())
                .exception(executionContext.wasSuccessful()? null : executionContext.getException().toString())
                .io(io)
                .steps(executionContext.getStepInsights().stream().map(ExecutionContext.StepInsight::toString).collect(Collectors.toList()))
                .build();
        var structuredFormat = UseCaseLogStructuredFormat.builder()
                .useCaseExecution(executionData)
                .build();
        return NativeExtractionMode.executeOn(structuredFormat);
    }

    private static String generateSimpleTextFormat(ExecutionContext executionContext) {
        var provider = AutologProvider.SINGLETON;
        boolean loggingUseCaseIO = provider.getUseCasesLoggingIO();
        return "Use case '"
                + executionContext.getSubject()
                + "' execution with correlation ID of '"
                + executionContext + "' took about "
                + executionContext.calculateLatency()
                + "ms and "
                + (executionContext.wasSuccessful()? "finished successfully." : ("threw an exception: " + executionContext.getException()))
                + " | Inner steps: "
                + NativeExtractionMode.executeOn(executionContext.getStepInsights().stream().map(ExecutionContext.StepInsight::toString).collect(Collectors.toList()))
                + (loggingUseCaseIO? Autolog.generateIOLog(executionContext) : "");
    }

    private static String generateIOLog(ExecutionContext executionContext) {
        var ioLoggingMode = AutologProvider.SINGLETON.getIOLoggingMode();
        var nativeExtraction = ioLoggingMode.equals(IOAutologMode.CAE_NATIVE);
        return Autolog.generateInputLog(executionContext, nativeExtraction) + Autolog.generateOutputLog(executionContext, nativeExtraction);
    }

    private static String generateInputLog(ExecutionContext executionContext, boolean nativeExtraction) {
        return Optional.ofNullable(executionContext.getInput())
                .map(input -> " | Input: " + (nativeExtraction? NativeExtractionMode.executeOn(input) : input.toString()))
                .orElse("");
    }

    private static String generateOutputLog(ExecutionContext executionContext, boolean nativeExtraction) {
        return Optional.ofNullable(executionContext.getOutput())
                .map(output -> " | Output: " + (nativeExtraction? NativeExtractionMode.executeOn(output) : output.toString()))
                .orElse("");
    }
}
