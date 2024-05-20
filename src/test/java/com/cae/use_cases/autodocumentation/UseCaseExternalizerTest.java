package com.cae.use_cases.autodocumentation;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;

@ExtendWith(MockitoExtension.class)
class UseCaseExternalizerTest {

    @BeforeEach
    void setUp(){
        var file = new File("target/use_cases.json");
        if (file.exists())
            //noinspection ResultOfMethodCallIgnored
            file.delete();
    }

    @Test
    void shouldRunTheRegistererWithoutExplodingMyPC(){
        Assertions.assertDoesNotThrow(() -> UseCaseDocumentationExternalizer.externalize(null));
        this.checkIfFileWasCreated();
    }

    void checkIfFileWasCreated(){
        var file = new File("target/use_cases.json");
        Assertions.assertTrue(file.exists());
    }

    @Test
    void shouldInstantiateExternalizeUseCaseExceptionCorrectly(){
        var someOriginalException = new RuntimeException("some not good stuff just happened");
        var exception = new UseCaseDocumentationExternalizer.ExternalizeUseCaseException(someOriginalException);
        Assertions.assertEquals("Something went wrong while trying to externalize use cases info. More details: " + someOriginalException, exception.getMessage());
    }

}
