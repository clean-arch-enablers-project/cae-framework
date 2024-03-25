package com.cae.use_cases.providers;

import com.cae.use_cases.UseCase;

/**
 * Interface for establishing a pattern for providing instances of
 * use cases. In case you are asking yourself about the UseCaseFactory
 * interface, this here is for a different purpose: the factory is only responsible
 * for instantiating the use case object, passing its required dependencies
 * without knowing which concrete classes are implementing the
 * abstractions. When it comes to the UseCaseProvider, its
 * responsibility is to manually inject, if necessary, all the concrete objets in the
 * UseCaseFactory so the factory will be able to instantiate correctly the
 * use case instance. So, the provider is meant to be 'dirtier' than the factory.
 * @param <U> use case type
 */
public interface UseCaseProvider<U extends UseCase>{

    /**
     * Method where all the concrete objects needed to instantiate
     * the use case are provided to the use case factory, and the use case
     * instance is returned directly.
     * @return the use case instance
     */
    U provideInstance();

}
