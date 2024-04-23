package com.cae.use_cases.registerers;

import com.cae.use_cases.UseCase;
import com.cae.use_cases.metadata.UseCaseMetadata;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class UseCaseRegistererTest {

    @Mock
    UseCase useCase1;
    @Mock
    UseCaseMetadata useCase1Metadata;
    @Mock
    UseCase useCase2;
    @Mock
    UseCaseMetadata useCase2Metadata;
    @Mock
    UseCase useCase3;
    @Mock
    UseCaseMetadata useCase3Metadata;

    @BeforeEach
    void setUp(){
        var file = new File("target/use_cases.txt");
        if (file.exists())
            //noinspection ResultOfMethodCallIgnored
            file.delete();
    }

    @Test
    void shouldRunTheRegistererWithoutExplodingMyPC(){
        Mockito.when(this.useCase1.getUseCaseMetadata()).thenReturn(this.useCase1Metadata);
        Mockito.when(this.useCase2.getUseCaseMetadata()).thenReturn(this.useCase2Metadata);
        Mockito.when(this.useCase3.getUseCaseMetadata()).thenReturn(this.useCase3Metadata);
        Assertions.assertDoesNotThrow(() -> UseCaseRegisterer.runOnUseCases(List.of(this.useCase1, this.useCase2, this.useCase3)));
        this.checkIfFileWasCreated();
    }

    @Test
    void shouldRunTheRegistererWithoutExplodingMyPCAgain(){
        Mockito.when(this.useCase1Metadata.getName()).thenReturn("use case 1");
        Mockito.when(this.useCase2Metadata.getName()).thenReturn("use case 2");
        Mockito.when(this.useCase3Metadata.getName()).thenReturn("use case 3");
        Assertions.assertDoesNotThrow(() -> UseCaseRegisterer.runOnUseCasesMetadata(List.of(this.useCase1Metadata, this.useCase2Metadata, this.useCase3Metadata)));
        this.checkIfFileWasCreated();
    }

    void checkIfFileWasCreated(){
        var file = new File("target/use_cases.txt");
        Assertions.assertTrue(file.exists());
    }

    @Test
    void shouldInstantiateExternalizeUseCaseExceptionCorrectly(){
        var someOriginalException = new RuntimeException("some not good stuff just happened");
        var exception = new UseCaseRegisterer.ExternalizeUseCaseException(someOriginalException);
        Assertions.assertEquals("Something went wrong while trying to externalize use cases info. More details: " + someOriginalException, exception.getMessage());
    }

}
