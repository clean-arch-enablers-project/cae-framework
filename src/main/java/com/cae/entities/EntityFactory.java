package com.cae.entities;

/**
 * This interface is for establishing the pattern of instantiating your
 * entity classes via Factories. If you have, for instance, an entity class
 * called Customer, it would be for you to create a factory responsible
 * for instantiating objects of it (e.g. CustomersFactory).
 * @param <E> E being a parameterized type which extends Entity
 */
public interface EntityFactory <E extends Entity>{

    /**
     * Method which is supposed to return the entity instance. Feel
     * free to add any necessary logic within, even creating more
     * specific methods within your implementations. The core idea
     * is to place all the instantiation methods inside implementations
     * of this interface.
     * @return the instance of your entity class
     */
    E makeNewInstance();

}
