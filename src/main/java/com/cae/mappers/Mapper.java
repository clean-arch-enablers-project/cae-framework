package com.cae.mappers;

/**
 * Interface for establishing pattern of defining classes with the
 * purpose of parsing objects from type A to type B. Implement
 * your mappers inheriting the contract provided by this interface.
 * @param <I> original type being parsed from
 * @param <O> target type being parsed to
 */
public interface Mapper<I, O>{

    /**
     * Principal method which receives an object of type A that will
     * be parsed to type B.
     * @param input object to parse
     * @return object parsed
     */
    O map(I input);

}
