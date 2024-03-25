package com.cae.use_cases.metadata;

import com.cae.loggers.Logger;
import com.cae.use_cases.UseCase;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UseCaseMetadataTest {

    private static class SomeUseCaseImplementation extends UseCase {

        protected SomeUseCaseImplementation(UseCaseMetadata useCaseMetadata, Logger logger) {
            super(useCaseMetadata, logger);
        }
    }

    @Test
    void shouldInstantiateCorrectlyWhenCallingTheProtectedUseCaseConstructor(){
        var useCaseDescription = "This is some use case implementation just for testing the metadata component";
        var useCaseMetadata = UseCaseMetadata.ofProtectedUseCase(SomeUseCaseImplementation.class, useCaseDescription);
        var expectedUseCaseNameRetrievedFromItsMetadata = "some_implementation";
        var expectedUseCaseProtectionStatusRetrievedFromItsMetadata = true;
        Assertions.assertEquals(expectedUseCaseProtectionStatusRetrievedFromItsMetadata, useCaseMetadata.isProtected());
        Assertions.assertEquals(expectedUseCaseNameRetrievedFromItsMetadata, useCaseMetadata.getName());
        Assertions.assertEquals(useCaseDescription, useCaseMetadata.getDescription());
    }

    @Test
    void shouldInstantiateCorrectlyWhenCallingTheOpenAccessUseCaseConstructor(){
        var useCaseDescription = "This is some use case implementation just for testing the metadata component";
        var useCaseMetadata = UseCaseMetadata.ofOpenAccessUseCase(SomeUseCaseImplementation.class, useCaseDescription);
        var expectedUseCaseNameRetrievedFromItsMetadata = "some_implementation";
        var expectedUseCaseProtectionStatusRetrievedFromItsMetadata = false;
        Assertions.assertEquals(expectedUseCaseProtectionStatusRetrievedFromItsMetadata, useCaseMetadata.isProtected());
        Assertions.assertEquals(expectedUseCaseNameRetrievedFromItsMetadata, useCaseMetadata.getName());
        Assertions.assertEquals(useCaseDescription, useCaseMetadata.getDescription());
    }

}
