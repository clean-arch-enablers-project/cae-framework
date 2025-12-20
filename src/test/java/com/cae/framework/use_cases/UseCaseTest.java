package com.cae.framework.use_cases;

import com.cae.framework.autofeatures.autolog.AutologProvider;
import com.cae.framework.autofeatures.autolog.Logger;
import com.cae.framework.use_cases.registries.UseCaseRegistry;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UseCaseTest {


    @Test
    @DisplayName("Should set the use case metadata during the instantiation")
    void shouldSetTheUseCaseMetadataDuringTheInstantiation(){
        var unit = new SomeBasicUseCase();
        Assertions.assertNotNull(unit.getUseCaseMetadata());
    }

    @Test
    @DisplayName("Should add the use case to the UseCaseRegistry when instantiated")
    void shouldAddTheUseCaseToTheUseCaseRegistryWhenInstantiated(){
        var unit = new SomeBasicUseCase();
        var unitIsInTheRegistry = UseCaseRegistry.SINGLETON.getRegisteredUseCases()
                .stream()
                .anyMatch(registered -> registered.equals(unit));
        Assertions.assertTrue(unitIsInTheRegistry);
    }

    @Test
    @DisplayName("Should throw InternalMappedException when Logger implementation is not provided")
    void shouldThrowInternalMappedExceptionWhenLoggerImplementationIsNotProvided(){
        AutologProvider.SINGLETON.setProvidedInstance(null);
        var unit = new SomeBasicUseCase();
        Assertions.assertThrows(InternalMappedException.class, unit::getLogger);
    }

    @Test
    @DisplayName("InternalMappedException should contain expected message when Logger implementation is not provided")
    void internalMappedExceptionShouldContainExpectedMessageWhenLoggerImplementationIsNotProvided(){
        try {
            AutologProvider.SINGLETON.setProvidedInstance(null);
            var unit = new SomeBasicUseCase();
            unit.getLogger();
        } catch (Exception exception){
            Assertions.assertInstanceOf(InternalMappedException.class, exception);
            var expectedBriefPublicMessage = "No logger instance provided for the use case \"SomeBasicUseCase\"";
            var expectedDetails = "Please provide an instance via the AutologProvider singleton instance";
            var internalMappedException = (InternalMappedException) exception;
            Assertions.assertEquals(expectedBriefPublicMessage, internalMappedException.getBriefPublicMessage());
            Assertions.assertTrue(internalMappedException.getDetails().isPresent());
            Assertions.assertEquals(expectedDetails, internalMappedException.getDetails().get());
        }
    }

    @Test
    @DisplayName("getLogger method should return the provided implementation instance")
    void getLoggerMethodShouldReturnTheProvidedImplementationInstance(){
        var mockedImplementationInstance = Mockito.mock(Logger.class);
        AutologProvider.SINGLETON.setProvidedInstance(mockedImplementationInstance);
        var unit = new SomeBasicUseCase();
        var returnedLogger = unit.getLogger();
        Assertions.assertEquals(mockedImplementationInstance, returnedLogger);
    }

    public static class SomeBasicUseCase extends UseCase {}

}
