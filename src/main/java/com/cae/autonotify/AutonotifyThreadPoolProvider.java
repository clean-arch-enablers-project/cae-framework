package com.cae.autonotify;

import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class AutonotifyThreadPoolProvider {

    public static final AutonotifyThreadPoolProvider SINGLETON = new AutonotifyThreadPoolProvider();

    private AutonotifyThreadPoolProvider(){
        Runtime.getRuntime()
                .addShutdownHook(new Thread(this::shutdown, "CaeAutonotifyThreadPool-ShutdownHook"));
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


    public AutonotifyThreadPoolProvider setExecutor(ExecutorService executor){
        this.executor = executor;
        return this;
    }

    public AutonotifyThreadPoolProvider setMinSize(Integer minSize){
        this.minSize = minSize;
        return this;
    }

    public AutonotifyThreadPoolProvider setMaxSize(Integer maxSize){
        this.maxSize = maxSize;
        return this;
    }

    public AutonotifyThreadPoolProvider setKeepAliveTimeForIdleThreadsInSeconds(Long keepAliveTimeForIdleThreadsInSeconds){
        this.keepAliveTimeForIdleThreadsInSeconds = keepAliveTimeForIdleThreadsInSeconds;
        return this;
    }

    public AutonotifyThreadPoolProvider setQueueCapacity(Integer queueCapacity){
        this.queueCapacity = queueCapacity;
        return this;
    }

    public AutonotifyThreadPoolProvider setPoolName(String poolName){
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
                new LinkedBlockingQueue<>(Optional.ofNullable(this.queueCapacity).orElse(100)),
                new CaeAutonotifyThreadFactory(Optional.ofNullable(this.poolName).orElse("CaeAutonotifyThreadPool")),
                new ThreadPoolExecutor.CallerRunsPolicy()
        );
    }

    protected static class CaeAutonotifyThreadFactory implements ThreadFactory {

        private final String namePrefix;
        private final AtomicInteger threadNumber = new AtomicInteger(1);

        protected CaeAutonotifyThreadFactory(String namePrefix) {
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