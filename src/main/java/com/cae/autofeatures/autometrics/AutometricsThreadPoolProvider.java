package com.cae.autofeatures.autometrics;

import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class AutometricsThreadPoolProvider {

    public static final AutometricsThreadPoolProvider SINGLETON = new AutometricsThreadPoolProvider();

    private AutometricsThreadPoolProvider(){
        Runtime.getRuntime()
                .addShutdownHook(new Thread(this::shutdown, "CaeAutometricsThreadPool-ShutdownHook"));
    }

    private void shutdown() {
        if (this.executor != null) {
            this.executor.shutdown();
            try {
                if (!this.executor.awaitTermination(60, TimeUnit.SECONDS)) {
                    this.executor.shutdownNow();
                }
            } catch (InterruptedException e) {
                this.executor.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }

    protected ExecutorService executor;
    protected Integer minSize;
    protected Integer maxSize;
    protected Long keepAliveTimeForIdleThreadsInSeconds;
    protected Integer queueCapacity;
    protected String poolName;


    public AutometricsThreadPoolProvider setExecutor(ExecutorService executor){
        this.executor = executor;
        return this;
    }

    public AutometricsThreadPoolProvider setMinSize(Integer minSize){
        this.minSize = minSize;
        return this;
    }

    public AutometricsThreadPoolProvider setMaxSize(Integer maxSize){
        this.maxSize = maxSize;
        return this;
    }

    public AutometricsThreadPoolProvider setKeepAliveTimeForIdleThreadsInSeconds(Long keepAliveTimeForIdleThreadsInSeconds){
        this.keepAliveTimeForIdleThreadsInSeconds = keepAliveTimeForIdleThreadsInSeconds;
        return this;
    }

    public AutometricsThreadPoolProvider setQueueCapacity(Integer queueCapacity){
        this.queueCapacity = queueCapacity;
        return this;
    }

    public AutometricsThreadPoolProvider setPoolName(String poolName){
        this.poolName = poolName;
        return this;
    }

    public ExecutorService getExecutor(){
        if (this.executor == null){
            this.executor = this.autoProvideExecutor();
        }
        return this.executor;
    }

    private ExecutorService autoProvideExecutor() {
        return new ThreadPoolExecutor(
                Optional.ofNullable(this.minSize).orElse(5),
                Optional.ofNullable(this.maxSize).orElse(20),
                Optional.ofNullable(this.keepAliveTimeForIdleThreadsInSeconds).orElse(60L),
                TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(Optional.ofNullable(this.queueCapacity).orElse(10)),
                new CaeAutometricsThreadFactory(Optional.ofNullable(this.poolName).orElse("CaeAutometricsThreadPool")),
                new ThreadPoolExecutor.CallerRunsPolicy()
        );
    }

    protected static class CaeAutometricsThreadFactory implements ThreadFactory {

        private final String namePrefix;
        private final AtomicInteger threadNumber = new AtomicInteger(1);

        protected CaeAutometricsThreadFactory(String namePrefix) {
            this.namePrefix = namePrefix;
        }

        @Override
        public Thread newThread(Runnable runnable) {
            var newThread = new Thread(runnable, this.namePrefix + " - " + this.threadNumber.getAndIncrement());
            newThread.setDaemon(false);
            return newThread;
        }
    }

}