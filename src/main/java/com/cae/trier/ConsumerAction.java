package com.cae.trier;

import java.util.function.Consumer;

/**
 * Actions that only have input, but no output.
 * @param <I> type of the input
 */
public class ConsumerAction<I> implements Action<I, Void>{

    /**
     * The action itself to be executed.
     */
    private final Consumer<I> consumer;

    ConsumerAction(Consumer<I> consumer) {
        this.consumer = consumer;
    }

    /**
     * Where the execution happens
     * @param input the input needed to execute the action itself
     * @return null as Consumers don't return anything
     */
    @Override
    public Void execute(I input) {
        this.consumer.accept(input);
        return null;
    }
}
