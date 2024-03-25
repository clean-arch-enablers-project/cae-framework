package com.cae.trier;

import java.util.function.Function;

/**
 * Actions that have both input and output.
 * @param <I> type of the input
 * @param <O> type of the output
 */
public class FunctionAction<I, O> implements Action<I, O> {

    /**
     * The action itself
     */
    private final Function<I, O> function;

    FunctionAction(Function<I, O> function) {
        this.function = function;
    }

    /**
     * Where the action execution happens
     * @param input the input needed to execute the action itself
     * @return the output provided by the action itself
     */
    @Override
    public O execute(I input) {
        return this.function.apply(input);
    }
}
