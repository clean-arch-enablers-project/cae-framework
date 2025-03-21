package com.cae.use_cases.metadata;

import com.cae.use_cases.UseCase;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;

@ExtendWith(MockitoExtension.class)
class UseCaseMetadataTest {

    private static class SomeUseCaseImplementation extends UseCase {}

    @BeforeEach
    void setup(){
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldInstantiateCorrectlyWhenCallingTheProtectedUseCaseConstructor(){
//        var useCaseMetadata = UseCaseMetadata.ofProtectedUseCase(SomeUseCaseImplementation.class, new String[]{"write", "save"});
//        var expectedUseCaseNameRetrievedFromItsMetadata = "some_implementation";
//        var expectedUseCaseProtectionStatusRetrievedFromItsMetadata = true;
//        Assertions.assertEquals(expectedUseCaseProtectionStatusRetrievedFromItsMetadata, useCaseMetadata.isProtected());
//        Assertions.assertEquals(expectedUseCaseNameRetrievedFromItsMetadata, useCaseMetadata.getName());
    }

    @Test
    void shouldInstantiateCorrectlyWhenCallingTheOpenAccessUseCaseConstructor(){
//        var useCaseMetadata = UseCaseMetadata.ofOpenAccessUseCase(SomeUseCaseImplementation.class);
//        var expectedUseCaseNameRetrievedFromItsMetadata = "some_implementation";
//        var expectedUseCaseProtectionStatusRetrievedFromItsMetadata = false;
//        Assertions.assertEquals(expectedUseCaseProtectionStatusRetrievedFromItsMetadata, useCaseMetadata.isProtected());
//        Assertions.assertEquals(expectedUseCaseNameRetrievedFromItsMetadata, useCaseMetadata.getName());
    }

}
