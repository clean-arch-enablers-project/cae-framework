package com.cae.ports.autolog;

import com.cae.autolog.AutologProvider;
import com.cae.autolog.IOLoggingHandler;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.concurrent.CompletableFuture;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class PortInsightsManager {

    private final String portName;

    public static PortInsightsManager of(String portName){
        return new PortInsightsManager(portName);
    }

    public void keepInsightOf(
            ExecutionContext context,
            Object input,
            Object output,
            Exception exception,
            Long latency){
        this.runSynchronouslyIfNecessary(CompletableFuture.runAsync(() -> {
            var loggerProvider = AutologProvider.SINGLETON;
            var ioInsightBuilder = new StringBuilder();
            if (Boolean.TRUE.equals(loggerProvider.getPortsLoggingIO())) ioInsightBuilder.append(this.generateIOInsightFor(input, output));
            var exceptionInsight = "";
            if (exception != null){
                exceptionInsight = " an exception has been thrown along the way: " + exception;
            }
            var fullInsight = this.portName +
                    "'s insights:" +
                    ioInsightBuilder +
                    " (" + latency + "ms)" +
                    (exceptionInsight.isBlank() ? " no exception has been thrown" : exceptionInsight);
            PortInsights.SINGLETON.register(context, fullInsight);
        }));
    }

    private void runSynchronouslyIfNecessary(CompletableFuture<Void> future){
        if (Boolean.FALSE.equals(AutologProvider.SINGLETON.getAsync()))
            future.join();
    }

    private String generateIOInsightFor(Object input, Object output) {
        return " " + this.generateInputPartOfLog(input) + this.generateOutputPartOfLog(output);
    }

    private String generateInputPartOfLog(Object input) {
        return (input == null ? "" : IOLoggingHandler.generateTextForLoggingInput(input, "PORT"));
    }

    private String generateOutputPartOfLog(Object output) {
        return (output == null ? "" : IOLoggingHandler.generateTextForLoggingOutput(output, "PORT"));
    }

}
