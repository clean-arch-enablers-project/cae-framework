package com.cae.autofeatures.autometrics;

import com.cae.initializers.Lazy;
import lombok.AccessLevel;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public class AutometricsProvider {

    public static final AutometricsProvider SINGLETON = new AutometricsProvider();

    @Getter
    private Boolean async = true;
    @Getter(AccessLevel.PROTECTED)
    private List<AutometricsSubscriber> subscribers = new ArrayList<>();
    @Getter
    private List<Lazy<AutometricsSubscriber>> lazySubscribers = new ArrayList<>();

    public AutometricsProvider setAsync(Boolean async){
        this.async = async;
        return this;
    }

    public AutometricsProvider subscribe(AutometricsSubscriber subscriber){
        this.subscribers.add(subscriber);
        return this;
    }

    public AutometricsProvider subscribe(Lazy<AutometricsSubscriber> subscriber){
        this.lazySubscribers.add(subscriber);
        return this;
    }

    public void reset(){
        this.subscribers = new ArrayList<>();
        this.lazySubscribers = new ArrayList<>();
        this.async = true;
    }

}
