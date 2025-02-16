package com.cae.use_cases.autodoc;

import com.cae.autodoc.Autodoc;
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

//    @Test
//    void shouldRunTheRegistererWithoutExplodingMyPC(){
//        var domainName = "Services";
//        Assertions.assertDoesNotThrow(() -> Autodoc.externalize(null, "Services"));
//        this.checkIfFileWasCreated(domainName);
//    }

    void checkIfFileWasCreated(String domainName){
        var file = new File("target/"+domainName.toLowerCase()+"-documentation.json");
        Assertions.assertTrue(file.exists());
    }

    @Test
    void shouldInstantiateExternalizeUseCaseExceptionCorrectly(){
        var someOriginalException = new RuntimeException("some not good stuff just happened");
        var exception = new Autodoc.ExternalizeUseCaseException(someOriginalException);
        Assertions.assertEquals("Something went wrong while trying to externalize use cases info. More details: " + someOriginalException, exception.getMessage());
    }

}
