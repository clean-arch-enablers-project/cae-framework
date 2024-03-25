package com.cae.trier;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The action factory which returns instances of Actions based on the
 * builtin type of method that is being passed as parameter.
 */
interface ActionFactory {

    /**
     * Receives a method with input and output and parses it to the
     * Action form of it (FunctionAction).
     * @param function the method which has input and output
     * @return the Action form of the method
     * @param <I> the type of the input
     * @param <O> the type of the output
     */
    static <I, O> Action<I, O> of(Function<I, O> function){
        return new FunctionAction<>(function);
    }

    /**
     * Receives a method with only input but no output and parses it
     * to the Action form of it (ConsumerAction).
     * @param consumer the method which has input and no output
     * @return the Action form of the method
     * @param <I> the type of the input
     */
    static <I> Action<I, Void> of(Consumer<I> consumer){
        return new ConsumerAction<>(consumer);
    }

    /**
     * Receives a method which only returns but accepts no input and
     * parses it to the Action form of it (SupplierAction).
     * @param supplier the method which has only output
     * @return the Action form of the method
     * @param <O> the type of the output
     */
    static <O> Action<Void, O> of(Supplier<O> supplier){
        return new SupplierAction<>(supplier);
    }

    /**
     * Receives any runnable (which are executables that have neither
     * input nor output) and parses it to the Action form of it
     * (RunnableAction).
     * @param runnable the runnable
     * @return the Action form of it
     */
    static Action<Void, Void> of(Runnable runnable){
        return new RunnableAction(runnable);
    }

}
