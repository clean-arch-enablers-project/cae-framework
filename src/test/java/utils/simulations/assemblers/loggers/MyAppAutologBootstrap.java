package utils.simulations.assemblers.loggers;

import com.cae.autolog.AutologProvider;
import com.cae.autolog.IOLoggingMode;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.loggers.LoggerAdapterForTesting;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MyAppAutologBootstrap {

    public static void startupDefaultSettings(){
        AutologProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setUseCasesLoggingIO(true)
                .setPortsLoggingIO(true)
                .structuredFormat(false)
                .setIOLoggingMode(IOLoggingMode.TO_STRING);
    }

    public static void startupSyncAllTrueSettingsAndNative(){
        AutologProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setUseCasesLoggingIO(true)
                .setPortsLoggingIO(true)
                .structuredFormat(true)
                .setIOLoggingMode(IOLoggingMode.CAE_NATIVE);
    }

}
