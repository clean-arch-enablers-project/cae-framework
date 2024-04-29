package utils;

import com.cae.loggers.IOLoggingMode;
import com.cae.loggers.LoggerProvider;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LoggerBootstrapForTesting {

    public static void startupDefaultSettings(){
        LoggerProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setUseCasesLoggingIO(true)
                .setPortsLoggingIO(true)
                .setIOLoggingMode(IOLoggingMode.TO_STRING);
    }

}
