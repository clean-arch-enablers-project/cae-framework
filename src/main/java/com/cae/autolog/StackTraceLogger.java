package com.cae.loggers;

import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class StackTraceLogger {

    public static final StackTraceLogger SINGLETON = new StackTraceLogger();

    public void handleLoggingStackTrace(Exception exception, ExecutionContext context, String name){
        if (Boolean.TRUE.equals(AutologProvider.SINGLETON.getLogStackTrace())){
            var logger = AutologProvider.SINGLETON.getProvidedInstance()
                    .orElseThrow(() -> new InternalMappedException(
                            "No logger instance was provided",
                            "Please make sure an instance of Logger is provided"
                    ));
            var mappedException = new MappedException("stack trace logging", "", exception);
            var linesOfStackTrace = mappedException.getLinesFromStackTraceFromOriginalException(AutologProvider.SINGLETON.getLinesOfStackTrace());
            var linesAsUniqueString = linesOfStackTrace.stream()
                    .reduce(
                            "From execution context of '" + context.toString() + "' at '" + name + "'",
                            (previous, next) -> previous.concat("\n\t\t").concat(next));
            logger.logError(linesAsUniqueString);
        }
    }

}
