package com.cae.autofeatures;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.Logger;
import com.cae.autofeatures.autometrics.AutometricsProvider;
import com.cae.autofeatures.autonotify.AutonotifyProvider;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;

import java.util.concurrent.ExecutorService;

@ExtendWith(MockitoExtension.class)
class PostExecutionAutofeaturesRunnerTest {

    @Mock
    Logger logger;

    ExecutorService autologExecutor;
    ExecutorService autometricsExecutor;
    ExecutorService autonotifyExecutor;

    @BeforeEach
    void setup(){
        MockedAutofeaturesRunnerProvider.run();
        AutologProvider.SINGLETON.reset();
        AutometricsProvider.SINGLETON.reset();
        AutonotifyProvider.SINGLETON.reset();
        AutologProvider.SINGLETON.setProvidedInstance(this.logger);
        this.autologExecutor = MockedAutofeaturesRunnerProvider.autologExecutor;
        this.autometricsExecutor = MockedAutofeaturesRunnerProvider.autometricsExecutor;
        this.autonotifyExecutor = MockedAutofeaturesRunnerProvider.autonotifyExecutor;
    }

    @Test
    @DisplayName("Should call all of the post-execution autofeatures' thread pool executors")
    void shouldCallAllOfThePostExecutionAutofeaturesThreadPoolExecutors(){
        Mockito.verify(this.autologExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(this.autometricsExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(this.autonotifyExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        PostExecutionAutofeaturesRunner.runOn(ExecutionContext.ofNew());
        Mockito.verify(this.autologExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(this.autometricsExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(this.autonotifyExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
    }

}
