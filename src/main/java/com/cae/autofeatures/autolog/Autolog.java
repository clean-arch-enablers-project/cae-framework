package com.cae.autofeatures.autolog;

import com.cae.autofeatures.autolog.formats.IO;
import com.cae.autofeatures.autolog.formats.SubjectLogStructuredFormat;
import com.cae.autofeatures.autolog.native_io_extraction_mode.NativeExtractionMode;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.ExecutionContext;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Autolog {

    private Autolog(){}

    public static void runOn(ExecutionContext executionContext) {
        var logger = Autolog.getProvidedLogger();
        Runnable action = () -> {
            Autolog.handleStackTraceOn(executionContext, logger);
            Autolog.writeBasedOn(executionContext, logger);
        };
        boolean shouldRunAsynchronously = AutologProvider.SINGLETON.getAsync();
        if (shouldRunAsynchronously)
            AutologThreadPoolProvider.SINGLETON.getExecutor().submit(action);
        else
            action.run();
    }

    protected static Logger getProvidedLogger() {
        return AutologProvider.SINGLETON.getProvidedInstance().orElseThrow(() -> new InternalMappedException(
                "Couldn't execute Autolog",
                "The framework does not provide a default Logger implementation, you must supply one yourself."
        ));
    }

    private static void handleStackTraceOn(ExecutionContext executionContext, Logger logger) {
        if (!executionContext.wasSuccessful()) StackTraceAutologAux.SINGLETON.handleLoggingStackTrace(executionContext, logger);
    }

    protected static void writeBasedOn(ExecutionContext executionContext, Logger logger) {
        Consumer<String> loggerAction = executionContext.wasSuccessful()? logger::logInfo : logger::logError;
        boolean isStructuredFormat = AutologProvider.SINGLETON.getStructuredFormat();
        loggerAction.accept(isStructuredFormat? Autolog.generateStructuredFormat(executionContext) : Autolog.generateSimpleTextFormat(executionContext));
    }

    private static String generateStructuredFormat(ExecutionContext executionContext) {
        var io = Boolean.FALSE.equals(AutologProvider.SINGLETON.getSubjectsLoggingIO())? null : IO.builder()
                .input(executionContext.getInput())
                .output(executionContext.getOutput())
                .build();
        var executionData = SubjectLogStructuredFormat.SubjectExecutionLogFormat.builder()
                .subject(executionContext.getSubject())
                .correlationId(executionContext.toString())
                .latency(executionContext.calculateLatency())
                .successful(executionContext.wasSuccessful())
                .exception(executionContext.wasSuccessful()? null : executionContext.getException().toString())
                .io(io)
                .steps(executionContext.getStepInsights().stream().map(Autolog::createStepRepresentation).collect(Collectors.toList()))
                .build();
        var structuredFormat = SubjectLogStructuredFormat.builder()
                .execution(executionData)
                .build();
        return NativeExtractionMode.executeOn(structuredFormat);
    }

    private static String generateSimpleTextFormat(ExecutionContext executionContext) {
        var provider = AutologProvider.SINGLETON;
        boolean loggingSubjectIO = provider.getSubjectsLoggingIO();
        return "Subject '"
                + executionContext.getSubject()
                + "' execution with correlation ID of '"
                + executionContext + "' took about "
                + executionContext.calculateLatency()
                + "ms and "
                + (executionContext.wasSuccessful()? "finished successfully." : ("threw an exception: " + executionContext.getException()))
                + " | Inner steps: "
                + NativeExtractionMode.executeOn(executionContext.getStepInsights().stream().map(Autolog::createStepRepresentation).collect(Collectors.toList()))
                + (loggingSubjectIO? Autolog.generateIOLog(executionContext) : "");
    }

    private static String createStepRepresentation(ExecutionContext.StepInsight step) {
        var toStringRepresentation = step.toString();
        if (AutologProvider.SINGLETON.getInnerStepsLoggingIO())
            return toStringRepresentation
                    + Optional.ofNullable(step.getInput()).map(input -> " | Input: " + NativeExtractionMode.executeOn(input)).orElse("")
                    + Optional.ofNullable(step.getOutput()).map(output -> " | Output: " + NativeExtractionMode.executeOn(output)).orElse("");
        return toStringRepresentation;
    }

    private static String generateIOLog(ExecutionContext executionContext) {
        var ioLoggingMode = AutologProvider.SINGLETON.getIoAutologMode();
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
