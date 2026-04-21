package utils;

import com.cae.framework.autofeatures.autolog.AutologThreadPoolProvider;
import com.cae.framework.autofeatures.autometrics.AutometricsThreadPoolProvider;
import com.cae.framework.autofeatures.autonotify.AutonotifyThreadPoolProvider;
import org.mockito.Mockito;

import java.util.concurrent.ExecutorService;

public class MockedAutofeaturesRunnerProvider {

    public static ExecutorService autologExecutor;
    public static ExecutorService autometricsExecutor;
    public static ExecutorService autonotifyExecutor;

    public static void run(){
        initializeMockedInstances();
        AutologThreadPoolProvider.SINGLETON.setExecutor(autologExecutor);
        AutometricsThreadPoolProvider.SINGLETON.setExecutor(autometricsExecutor);
        AutonotifyThreadPoolProvider.SINGLETON.setExecutor(autonotifyExecutor);
    }

    private static void initializeMockedInstances() {
        autologExecutor = Mockito.mock(ExecutorService.class);
        autometricsExecutor = Mockito.mock(ExecutorService.class);
        autonotifyExecutor = Mockito.mock(ExecutorService.class);
    }

    public static void flushMockedInstances(){
        AutologThreadPoolProvider.SINGLETON.setExecutor(null);
        AutometricsThreadPoolProvider.SINGLETON.setExecutor(null);
        AutonotifyThreadPoolProvider.SINGLETON.setExecutor(null);
        autologExecutor = null;
        autometricsExecutor = null;
        autonotifyExecutor = null;
    }

}
