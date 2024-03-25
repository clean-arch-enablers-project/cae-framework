package com.cae.trier;

/**
 * Interface that represents an action which has an input and an output
 * defined.
 * @param <I> the type of the input
 * @param <O> the type of the output
 */
public interface Action <I, O>{
    O execute(I input);

}
