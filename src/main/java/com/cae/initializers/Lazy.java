package com.cae.initializers;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.function.Supplier;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class Lazy<T> {

    public static <T> Lazy<T> loadingOf(Supplier<T> supplier){
        return new Lazy<>(supplier);
    }

    private final Supplier<T> lazySupplier;
    private volatile T instance;

    public T get(){
        T result = this.instance;
        if (result == null) {
            synchronized (this) {
                result = this.instance;
                if (result == null) {
                    result = this.lazySupplier.get();
                    this.instance = result;
                }
            }
        }
        return result;
    }

    public boolean isInitialized(){
        T result = this.instance;
        return result != null;
    }

    public void reset(){
        synchronized (this){
            this.instance = null;
        }
    }

}
