package com.cae.autofeatures.autometrics;

import lombok.AccessLevel;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public class AutometricsProvider {

    public static final AutometricsProvider SINGLETON = new AutometricsProvider();

    @Getter(AccessLevel.PROTECTED)
    private final List<AutometricsSubscriber> subscribers = new ArrayList<>();

    public AutometricsProvider subscribe(AutometricsSubscriber subscriber){
        this.subscribers.add(subscriber);
        return this;
    }

}
