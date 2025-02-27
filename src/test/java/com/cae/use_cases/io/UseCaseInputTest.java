package com.cae.use_cases.io;

import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import com.cae.use_cases.io.annotations.ValidInnerPropertiesInputField;
import com.cae.use_cases.io.exceptions.*;
import lombok.Getter;
import lombok.Setter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;

import java.util.Optional;

@ExtendWith(MockitoExtension.class)
class UseCaseInputTest {

    private SomeNormalUseCaseInputImplementation normalUseCase;
    @BeforeEach
    void setUp(){
        this.normalUseCase = new SomeNormalUseCaseInputImplementation();
        this.normalUseCase.setFieldWhichMustNotBeEmpty("Not empty string");
        this.normalUseCase.setFieldWhichMustNotBeBlank("Not blank field");
        this.normalUseCase.setFieldWhichMustNotBeNull(3);
        var innerObject = new SomeNormalInnerUseCaseInput();
        innerObject.setInnerFieldWhichMustNotBeNull(1);
        this.normalUseCase.setFieldWhichMustHaveItsPropertiesValid(innerObject);
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldValidateWithoutProblemsTheUseCaseInputImplementation(){
        Assertions.assertDoesNotThrow(this.normalUseCase::autoverify);
    }

    @Test
    void shouldThrowNullFieldExceptionWhenValidatingUseCaseInputImplementation(){
        this.normalUseCase.setFieldWhichMustNotBeNull(null);
        Assertions.assertThrows(NullFieldException.class, this.normalUseCase::autoverify);
    }

    @Test
    void shouldThrowBlankFieldExceptionWhenValidatingUseCaseInputImplementation(){
        this.normalUseCase.setFieldWhichMustNotBeBlank("    ");
        Assertions.assertThrows(BlankFieldException.class, this.normalUseCase::autoverify);
    }

    @Test
    void shouldThrowEmptyFieldExceptionWhenValidatingUseCaseInputImplementation(){
        this.normalUseCase.setFieldWhichMustNotBeEmpty("");
        Assertions.assertThrows(EmptyFieldException.class, this.normalUseCase::autoverify);
    }

    @Test
    void shouldThrowNullFieldExceptionWhenValidatingInnerPropertiesOfAPropertyOfTheUseCaseInputImplementation(){
        this.normalUseCase.getFieldWhichMustHaveItsPropertiesValid().setInnerFieldWhichMustNotBeNull(null);
        Assertions.assertThrows(NullFieldException.class, this.normalUseCase::autoverify);
    }

    @Test
    void shouldThrowNotBlankAnnotationOnWrongTypeException(){
        var input = new SomeProblematicInputWithNotBlankAnnotation();
        input.setSomeInteger(1);
        Assertions.assertThrows(NotBlankAnnotationOnWrongTypeException.class, input::autoverify);
    }

    @Test
    void shouldThrowNotEmptyAnnotationOnWrongTypeException(){
        var input = new SomeProblematicInputWithNotEmptyAnnotation();
        input.setSomeInteger(1);
        Assertions.assertThrows(NotEmptyAnnotationOnWrongTypeException.class, input::autoverify);
    }

    @Test
    void shouldThrowValidInnerPropertiesAnnotationOnWrongTypeException(){
        var input = new SomeProblematicInputWithValidInnerPropertiesAnnotation();
        input.setSomeInteger(1);
        Assertions.assertThrows(ValidInnerPropertiesAnnotationOnWrongTypeException.class, input::autoverify);
    }

    @Test
    void shouldThrowGetterMethodNotFoundException(){
        var input = new SomeProblematicInputWithNoGetterMethods();
        Assertions.assertThrows(UseCaseInput.GetterMethodNotFoundException.class, input::autoverify);
    }

    @Test
    void shouldThrowInternalMappedException(){
        var input = new SomeProblematicInputWithGetterMethod();
        Assertions.assertThrows(InternalMappedException.class, input::autoverify);
    }

    @Test
    void shouldThrowAtLeastOneOfTheFieldsIsSupposedToBeFilledException(){
        var inputWithArbitraryValidationRules = new SomeUseCaseInputThatImplementsArbitrarilyValidationRules();
        Assertions.assertThrows(SomeUseCaseInputThatImplementsArbitrarilyValidationRules.AtLeastOneOfTheFieldsIsSupposedToBeFilledException.class, inputWithArbitraryValidationRules::autoverify);
    }

    @Getter
    @Setter
    private static class SomeNormalUseCaseInputImplementation extends UseCaseInput{

        @NotEmptyInputField
        private String fieldWhichMustNotBeEmpty;
        @NotBlankInputField
        private String fieldWhichMustNotBeBlank;
        @NotNullInputField
        private Integer fieldWhichMustNotBeNull;
        @ValidInnerPropertiesInputField
        private SomeNormalInnerUseCaseInput fieldWhichMustHaveItsPropertiesValid;

    }

    @Getter
    @Setter
    private static class SomeNormalInnerUseCaseInput extends UseCaseInput{

        @NotNullInputField
        private Integer innerFieldWhichMustNotBeNull;

    }

    @Getter
    @Setter
    private static class SomeProblematicInputWithNotBlankAnnotation extends UseCaseInput{

        @NotBlankInputField
        private Integer someInteger;

    }

    @Getter
    @Setter
    private static class SomeProblematicInputWithNotEmptyAnnotation extends UseCaseInput{

        @NotEmptyInputField
        private Integer someInteger;

    }

    @Getter
    @Setter
    private static class SomeProblematicInputWithValidInnerPropertiesAnnotation extends UseCaseInput{

        @ValidInnerPropertiesInputField
        private Integer someInteger;

    }

    private static class SomeProblematicInputWithNoGetterMethods extends UseCaseInput{

        public final Integer someFieldWithoutGetter = 1;

    }

    private static class SomeProblematicInputWithGetterMethod extends UseCaseInput{

        @NotNullInputField
        private Integer fieldWithProblematicGetterMethod;

        public Integer getFieldWithProblematicGetterMethod(){
            throw new RuntimeException("opsie!");
        }

    }

    @Getter
    @Setter
    private static class SomeUseCaseInputThatImplementsArbitrarilyValidationRules extends UseCaseInput{

        private Integer someField;
        private Integer someOtherField;


        @Override
        public void validatePropertiesArbitrarily(){
            if (Optional.ofNullable(this.someField).isEmpty() && Optional.ofNullable(this.someOtherField).isEmpty())
                throw new AtLeastOneOfTheFieldsIsSupposedToBeFilledException();
        }

        public static class AtLeastOneOfTheFieldsIsSupposedToBeFilledException extends MappedException {
            public AtLeastOneOfTheFieldsIsSupposedToBeFilledException() {
                super("At least one of the input fields was supposed to be filled");
            }
        }
    }

}
