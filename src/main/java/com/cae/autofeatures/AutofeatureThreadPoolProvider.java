package com.cae.autofeatures;

import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public abstract class AutofeatureThreadPoolProvider {

    protected AutofeatureThreadPoolProvider(String poolName){
        Runtime.getRuntime()
                .addShutdownHook(new Thread(this::shutdown, poolName + "-ShutdownHook"));
        this.poolName = poolName;
    }

    private void shutdown() {
        if (this.executor != null) {
            this.executor.shutdown();
            try {
                if (!this.executor.awaitTermination(8, TimeUnit.SECONDS)) {
                    this.executor.shutdownNow();
                }
            } catch (InterruptedException e) {
                this.executor.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }

    protected final String poolName;
    protected ExecutorService executor;
    protected Integer minSize;
    protected Integer maxSize;
    protected Long keepAliveTimeForIdleThreadsInSeconds;
    protected Integer queueCapacity;


    public AutofeatureThreadPoolProvider setExecutor(ExecutorService executor){
        this.executor = executor;
        return this;
    }

    public AutofeatureThreadPoolProvider setMinSize(Integer minSize){
        this.minSize = minSize;
        return this;
    }

    public AutofeatureThreadPoolProvider setMaxSize(Integer maxSize){
        this.maxSize = maxSize;
        return this;
    }

    public AutofeatureThreadPoolProvider setKeepAliveTimeForIdleThreadsInSeconds(Long keepAliveTimeForIdleThreadsInSeconds){
        this.keepAliveTimeForIdleThreadsInSeconds = keepAliveTimeForIdleThreadsInSeconds;
        return this;
    }

    public AutofeatureThreadPoolProvider setQueueCapacity(Integer queueCapacity){
        this.queueCapacity = queueCapacity;
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
                Optional.ofNullable(this.maxSize).orElse(30),
                Optional.ofNullable(this.keepAliveTimeForIdleThreadsInSeconds).orElse(60L),
                TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(Optional.ofNullable(this.queueCapacity).orElse(100)),
                new AutofeatureThreadFactory(this.poolName),
                new ThreadPoolExecutor.CallerRunsPolicy()
        );
    }

    protected static class AutofeatureThreadFactory implements ThreadFactory {

        private final String namePrefix;
        private final AtomicInteger threadNumber = new AtomicInteger(1);

        protected AutofeatureThreadFactory(String namePrefix) {
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
