package utils.simulations.assemblers.loggers;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.IOAutologMode;
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
                .setIOLoggingMode(IOAutologMode.TO_STRING);
    }

    public static void startupSyncAllTrueSettingsAndNative(){
        AutologProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setUseCasesLoggingIO(true)
                .setPortsLoggingIO(true)
                .structuredFormat(true)
                .setIOLoggingMode(IOAutologMode.CAE_NATIVE);
    }

}
