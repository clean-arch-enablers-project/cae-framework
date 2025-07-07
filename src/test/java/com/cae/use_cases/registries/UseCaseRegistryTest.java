package com.cae.use_cases.registries;

import com.cae.use_cases.UseCase;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UseCaseRegistryTest {

    @Test
    @DisplayName("Should be able to add UseCase instances to the registry")
    void shouldBeAbleToAddUseCaseInstancesToTheRegistry(){
        var useCase = Mockito.mock(UseCase.class);
        var didNotHaveTheNewUseCaseBeforeRegistering = UseCaseRegistry.SINGLETON
                .getRegisteredUseCases()
                .stream()
                .noneMatch(registered -> registered.equals(useCase));
        UseCaseRegistry.SINGLETON.add(useCase);
        var hasTheNewUseCaseAfterRegistering = UseCaseRegistry.SINGLETON
                .getRegisteredUseCases()
                .stream()
                .anyMatch(registered -> registered.equals(useCase));
        Assertions.assertTrue(didNotHaveTheNewUseCaseBeforeRegistering);
        Assertions.assertTrue(hasTheNewUseCaseAfterRegistering);
    }

    @Test
    @DisplayName("Should be able to add interested")
    void shouldBeAbleToAddInterested(){
        var interested = Mockito.mock(UseCaseRegistry.UseCaseRegistryInterested.class);
        var didNotHaveTheInterestedBeforeAdding = UseCaseRegistry.SINGLETON
            .interestedParties
            .stream()
            .noneMatch(registered -> registered.equals(interested));
        UseCaseRegistry.SINGLETON.registerInterest(interested);
        var hasTheInterestedAfterAdding = UseCaseRegistry.SINGLETON
                .interestedParties
                .stream()
                .anyMatch(registered -> registered.equals(interested));
        Assertions.assertTrue(didNotHaveTheInterestedBeforeAdding);
        Assertions.assertTrue(hasTheInterestedAfterAdding);
    }

    @Test
    @DisplayName("Should notify each interested everytime a new UseCase is added")
    void shouldNotifyEachInterestedEverytimeANewUseCaseIsAdded(){
        var useCaseA = Mockito.mock(UseCase.class);
        var useCaseB = Mockito.mock(UseCase.class);
        var interestedA = Mockito.mock(UseCaseRegistry.UseCaseRegistryInterested.class);
        var interestedB = Mockito.mock(UseCaseRegistry.UseCaseRegistryInterested.class);
        UseCaseRegistry.SINGLETON
                .registerInterest(interestedA)
                .registerInterest(interestedB);
        Mockito.verify(interestedA, Mockito.times(0)).notifyNewUseCaseRegistered(useCaseA);
        Mockito.verify(interestedA, Mockito.times(0)).notifyNewUseCaseRegistered(useCaseB);
        Mockito.verify(interestedB, Mockito.times(0)).notifyNewUseCaseRegistered(useCaseA);
        Mockito.verify(interestedB, Mockito.times(0)).notifyNewUseCaseRegistered(useCaseB);
        UseCaseRegistry.SINGLETON.add(useCaseA);
        UseCaseRegistry.SINGLETON.add(useCaseB);
        Mockito.verify(interestedA, Mockito.times(1)).notifyNewUseCaseRegistered(useCaseA);
        Mockito.verify(interestedA, Mockito.times(1)).notifyNewUseCaseRegistered(useCaseB);
        Mockito.verify(interestedB, Mockito.times(1)).notifyNewUseCaseRegistered(useCaseA);
        Mockito.verify(interestedB, Mockito.times(1)).notifyNewUseCaseRegistered(useCaseB);
    }

}
