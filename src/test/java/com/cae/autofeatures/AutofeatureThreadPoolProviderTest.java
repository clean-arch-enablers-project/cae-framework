package com.cae.autofeatures;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@ExtendWith(MockitoExtension.class)
class AutofeatureThreadPoolProviderTest {

    private SomeAutofeatureThreadPoolProvider unit;

    @BeforeEach
    void setup(){
        this.unit = new SomeAutofeatureThreadPoolProvider();
    }

    @Test
    @DisplayName("Should be able to set executor")
    void shouldBeAbleToSetExecutor(){
        var executor = Mockito.mock(ExecutorService.class);
        var provider = this.unit.setExecutor(executor);
        Assertions.assertEquals(this.unit, provider);
        Assertions.assertEquals(executor, provider.executor);
    }

    @Test
    @DisplayName("Should be able to set min size")
    void shouldBeAbleToSetMinSize(){
        var minSize = 100;
        var provider = this.unit.setMinSize(minSize);
        Assertions.assertEquals(this.unit, provider);
        Assertions.assertEquals(minSize, provider.minSize);
    }

    @Test
    @DisplayName("Should be able to set max size")
    void shouldBeAbleToSetMaxSize(){
        var maxSize = 1000;
        var provider = this.unit.setMaxSize(maxSize);
        Assertions.assertEquals(this.unit, provider);
        Assertions.assertEquals(maxSize, provider.maxSize);
    }

    @Test
    @DisplayName("Should be able to set keepAliveTimeForIdleThreadsInSeconds")
    void shouldBeAbleToSetKeepAliveTimeForIdleThreadsInSeconds(){
        var keepAliveTimeForIdleThreadsInSeconds = 120L;
        var provider = this.unit.setKeepAliveTimeForIdleThreadsInSeconds(keepAliveTimeForIdleThreadsInSeconds);
        Assertions.assertEquals(this.unit, provider);
        Assertions.assertEquals(keepAliveTimeForIdleThreadsInSeconds, provider.keepAliveTimeForIdleThreadsInSeconds);
    }

    @Test
    @DisplayName("Should be able to set queueCapacity")
    void shouldBeAbleToSetQueueCapacity(){
        var queueCapacity = 2222;
        var provider = this.unit.setQueueCapacity(queueCapacity);
        Assertions.assertEquals(this.unit, provider);
        Assertions.assertEquals(queueCapacity, provider.queueCapacity);
    }

    @Test
    @DisplayName("When no executor is provided should return default one")
    void whenExecutorIsProvidedShouldReturnDefaultOne(){
        var defaultCoreSize = 5;
        var defaultMaxSize = 30;
        var defaultKeepAliveTimeForIdleThreadsInSeconds = 60L;
        var defaultQueueCapacity = 100;
        var executor = (ThreadPoolExecutor) this.unit.getExecutor();
        Assertions.assertEquals(defaultCoreSize, executor.getCorePoolSize());
        Assertions.assertEquals(defaultMaxSize, executor.getMaximumPoolSize());
        Assertions.assertEquals(defaultKeepAliveTimeForIdleThreadsInSeconds, executor.getKeepAliveTime(TimeUnit.SECONDS));
        Assertions.assertEquals(defaultQueueCapacity, executor.getQueue().remainingCapacity());
    }

    public static class SomeAutofeatureThreadPoolProvider extends AutofeatureThreadPoolProvider{
        protected SomeAutofeatureThreadPoolProvider() {
            super("SomeAutofeatureThreadPoolProvider");
        }
    }

}
