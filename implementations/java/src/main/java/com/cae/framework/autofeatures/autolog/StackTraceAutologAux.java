package com.cae.framework.autofeatures.autolog;

import com.cae.context.ExecutionContext;
import com.cae.mapped_exceptions.MappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class StackTraceAutologAux {

    public static final StackTraceAutologAux SINGLETON = new StackTraceAutologAux();

    public void handleLoggingStackTrace(ExecutionContext context, Logger logger){
        if (Boolean.TRUE.equals(AutologProvider.SINGLETON.getLogStackTrace())){
            var exception = context.getException();
            var name = context.getSubject();
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
