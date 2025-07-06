package com.cae.autofeatures.autolog;

import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class StackTraceAutologAux {

    public static final StackTraceAutologAux SINGLETON = new StackTraceAutologAux();

    public void handleLoggingStackTrace(ExecutionContext context){
        if (Boolean.TRUE.equals(AutologProvider.SINGLETON.getLogStackTrace())){
            var exception = context.getException();
            var name = context.getSubject();
            var logger = AutologProvider.SINGLETON.getProvidedInstance()
                    .orElseThrow(() -> new InternalMappedException(
                            "No logger instance was provided",
                            "Please make sure an instance of Logger is provided"
                    ));
            var mappedException = new MappedException("stack trace logging", "", exception);
            var linesOfStackTrace = mappedException.getLinesFromStackTraceFromOriginalException(AutologProvider.SINGLETON.getLinesOfStackTrace());
            var linesAsUniqueString = linesOfStackTrace.stream()
                    .reduce(
                            "From execution context of '" + context + "' at '" + name + "'",
                            (previous, next) -> previous.concat("\n\t\t").concat(next));
            logger.logError(linesAsUniqueString);
        }
    }

}
