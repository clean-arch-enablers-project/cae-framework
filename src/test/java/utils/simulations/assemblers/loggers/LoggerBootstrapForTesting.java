package utils.simulations.assemblers.loggers;

import com.cae.loggers.IOLoggingMode;
import com.cae.loggers.LoggerProvider;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.loggers.LoggerAdapterForTesting;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LoggerBootstrapForTesting {

    public static void startupDefaultSettings(){
        LoggerProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setUseCasesLoggingIO(true)
                .setPortsLoggingIO(true)
                .structuredFormat(false)
                .async(false)
                .setIOLoggingMode(IOLoggingMode.TO_STRING);
    }

}
