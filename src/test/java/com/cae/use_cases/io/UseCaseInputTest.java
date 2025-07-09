package com.cae.use_cases.io;

import com.cae.autofeatures.autoauth.annotations.ResourceIdentifier;
import com.cae.autofeatures.autoauth.annotations.ResourceOwnerIdentifier;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import com.cae.use_cases.io.annotations.ValidInnerPropertiesInputField;
import com.cae.use_cases.io.exceptions.*;
import lombok.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class UseCaseInputTest {

    private SomeUseCaseInput input;

    @BeforeEach
    void setup(){
        this.input = SomeUseCaseInput.builder()
            .someNotNullField("Abcd")
            .someNotBlankField("Efgh")
            .someNotEmptyField("Klmn")
            .someNotBlankStringListField(List.of("opqr", "stuv"))
            .someNotEmptyListField(List.of(1, 2))
            .innerInputProperties(InnerInputProperties.of(UUID.randomUUID()))
            .chainedInnerProperties(List.of(InnerInputProperties.of(UUID.randomUUID()), InnerInputProperties.of(UUID.randomUUID())))
            .build();
    }

    @Test
    @DisplayName("Should not throw when input object is alright")
    void shouldNotThrowWhenInputObjectIsAlright(){
        Assertions.assertDoesNotThrow(this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw BlankFieldException when a NotBlankInputField field is left blank")
    void shouldThrowBlankFieldExceptionWhenANotBlankInputFieldFieldIsLeftBlank(){
        this.input.setSomeNotBlankField("   ");
        Assertions.assertThrows(BlankFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw EmptyFieldException when a NotEmptyInputField field is left blank")
    void shouldThrowEmptyFieldExceptionWhenANotEmptyInputFieldFieldIsLeftEmpty(){
        this.input.setSomeNotEmptyField("");
        Assertions.assertThrows(EmptyFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw NotBlankAnnotationOnWrongTypeException when used on a wrong type")
    void shouldThrowNotBlankAnnotationOnWrongTypeExceptionWhenUsedOnAWrongType(){
        var anotherInput = new SomeInputWithNotBlankAnnotationsOnWrongType();
        anotherInput.setSomeNumber(1L);
        Assertions.assertThrows(NotBlankAnnotationOnWrongTypeException.class, anotherInput::autoverify);
    }

    @Test
    @DisplayName("Should throw NotBlankAnnotationOnWrongTypeException when used on a list of non-string items")
    void shouldThrowNotBlankAnnotationOnWrongTypeExceptionWhenUsedOnAListOfNonStringItems(){
        var anotherInput = new SomeInputWithNotBlankAnnotationsOnListOfNonStringItems();
        anotherInput.setSomeNumbers(List.of(1L, 2L));
        Assertions.assertThrows(NotBlankAnnotationOnWrongTypeException.class, anotherInput::autoverify);
    }

    @Test
    @DisplayName("Should throw NotEmptyAnnotationOnWrongTypeException when used on a wrong type")
    void shouldThrowNotEmptyAnnotationOnWrongTypeExceptionWhenUsedOnAWrongType(){
        var anotherInput = new SomeInputWithNotEmptyAnnotationsOnWrongType();
        anotherInput.setSomeNumber(10);
        Assertions.assertThrows(NotEmptyAnnotationOnWrongTypeException.class, anotherInput::autoverify);
    }

    @Test
    @DisplayName("Should throw NullFieldException when a NotNullInputField field is left null")
    void shouldThrowNullFieldExceptionWhenANotNullInputFieldFieldIsLeftNull(){
        this.input.setSomeNotNullField(null);
        Assertions.assertThrows(NullFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw ValidInnerPropertiesAnnotationOnWrongTypeException when used on a wrong type")
    void shouldThrowValidInnerPropertiesAnnotationOnWrongTypeExceptionWhenUsedOnAWrongType(){
        var anotherInput = new SomeInputWithValidInnerPropertiesOnWrongType();
        anotherInput.setSomeNumber(10);
        Assertions.assertThrows(ValidInnerPropertiesAnnotationOnWrongTypeException.class, anotherInput::autoverify);
    }

    @Test
    @DisplayName("Should throw BlankFieldException when list items are blank")
    void shouldThrowBlankFieldExceptionWhenListItemsAreBlank(){
        this.input.setSomeNotBlankStringListField(List.of("1", "3","   ", "4"));
        Assertions.assertThrows(BlankFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw EmptyFieldException when a list is empty")
    void shouldThrowEmptyFieldExceptionWhenListIsEmpty(){
        this.input.setSomeNotEmptyListField(new ArrayList<>());
        Assertions.assertThrows(EmptyFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw NullFieldException when a inner property of one of the fields is null")
    void shouldThrowNullFieldExceptionWhenInnerPropertyOfOneOfTheFieldsIsNull(){
        this.input.getInnerInputProperties().setSomeNotNullUuid(null);
        Assertions.assertThrows(NullFieldException.class, this.input::autoverify);
    }

    @Test
    @DisplayName("Should throw ValidInnerPropertiesAnnotationOnWrongTypeException when used on a list of non-use case items")
    void shouldThrowValidInnerPropertiesAnnotationOnWrongTypeExceptionWhenUsedOnAListOfNonUseCaseItems(){
        var someInput = new SomeInputWithValidInnerPropertiesOnListOfNonUseCaseItems();
        someInput.setSomeNumbers(List.of(1, 2, 3, 4));
        Assertions.assertThrows(ValidInnerPropertiesAnnotationOnWrongTypeException.class, someInput::autoverify);
    }

    @Test
    @DisplayName("Should throw GetterMethodNotFoundException when the input has no accessible getters for its properties")
    void shouldThrowGetterMethodNotFoundExceptionWhenTheInputHasNoAccessibleGettersForItsProperties(){
        var someInput = new SomeInputWithNoPublicGetterForItsProperties();
        someInput.setSomeField("1");
        Assertions.assertThrows(UseCaseInput.GetterMethodNotFoundException.class, someInput::autoverify);
    }

    @Test
    @DisplayName("Should be able to return the resource identifier as expected")
    void shouldBeAbleToReturnTheResourceIdentifierAsExpected(){
        var input = new SomeInputWithRBACIdentifiers();
        var expectedValue = "22";
        input.setSomeId(expectedValue);
        var actualReturn = input.getResourceIdentifier();
        Assertions.assertTrue(actualReturn.isPresent());
        Assertions.assertEquals(expectedValue, actualReturn.get());
    }

    @Test
    @DisplayName("Should be able to return the resource owner identifier as expected")
    void shouldBeAbleToReturnTheResourceOwnerIdentifierAsExpected(){
        var input = new SomeInputWithRBACIdentifiers();
        var expectedValue = "11";
        input.setSomeOtherId(expectedValue);
        var actualReturn = input.getResourceOwnerIdentifier();
        Assertions.assertTrue(actualReturn.isPresent());
        Assertions.assertEquals(expectedValue, actualReturn.get());
    }

    @Test
    @DisplayName("Should return empty Optional when there is no field annotated with ResourceIdentifier")
    void shouldReturnEmptyOptionalWhenThereIsNoFieldAnnotatedWithResourceIdentifier(){
        var result = this.input.getResourceIdentifier();
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return empty Optional when there is no field annotated with ResourceOwnerIdentifier")
    void shouldReturnEmptyOptionalWhenThereIsNoFieldAnnotatedWithResourceOwnerIdentifier(){
        var result = this.input.getResourceOwnerIdentifier();
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("When a getter of a field throws, should be able to map the exception to InternalMappedException")
    void whenGetterOfFieldThrowsShouldBeAbleToMapTheExceptionToInternalMappedException(){
        var someInput = new SomeInputWithAProblematicGetter(1L, 2L);
        Assertions.assertThrows(InternalMappedException.class, someInput::autoverify);
    }

    @Test
    @DisplayName("When a getter of a ResourceIdentifier field throws, should be able to map the exception to InternalMappedException")
    void whenGetterOfResourceIdentifierFieldThrowsShouldBeAbleToMapTheExceptionToInternalMappedException(){
        var someInput = new SomeInputWithAProblematicGetter(1L, 2L);
        Assertions.assertThrows(RuntimeException.class, someInput::getSomeField);
        Assertions.assertThrows(InternalMappedException.class, someInput::getResourceIdentifier);
    }

    @Test
    @DisplayName("When a getter of a ResourceOwnerIdentifier field throws, should be able to map the exception to InternalMappedException")
    void whenGetterOfResourceOwnerIdentifierFieldThrowsShouldBeAbleToMapTheExceptionToInternalMappedException(){
        var someInput = new SomeInputWithAProblematicGetter(1L, 2L);
        Assertions.assertThrows(RuntimeException.class, someInput::getAnotherField);
        Assertions.assertThrows(InternalMappedException.class, someInput::getResourceOwnerIdentifier);
    }

    @Getter
    @Setter
    @Builder
    public static class SomeUseCaseInput extends UseCaseInput{

        @NotNullInputField
        private String someNotNullField;

        @NotBlankInputField
        private String someNotBlankField;

        @NotEmptyInputField
        private String someNotEmptyField;

        @NotBlankInputField
        private List<String> someNotBlankStringListField;

        @NotEmptyInputField
        private List<Integer> someNotEmptyListField;

        @ValidInnerPropertiesInputField
        private InnerInputProperties innerInputProperties;

        @ValidInnerPropertiesInputField
        private List<InnerInputProperties> chainedInnerProperties;

    }

    @Getter
    @Setter
    @Builder
    public static class InnerInputProperties extends UseCaseInput{

        public static InnerInputProperties of(UUID uuid){
            return InnerInputProperties.builder().someNotNullUuid(uuid).build();
        }

        @NotNullInputField
        private UUID someNotNullUuid;

    }

    @Getter
    @Setter
    public static class SomeInputWithNotBlankAnnotationsOnWrongType extends UseCaseInput{
        @NotBlankInputField
        private Long someNumber;
    }

    @Getter
    @Setter
    public static class SomeInputWithNotBlankAnnotationsOnListOfNonStringItems extends UseCaseInput{
        @NotBlankInputField
        private List<Long> someNumbers;
    }

    @Getter
    @Setter
    public static class SomeInputWithNotEmptyAnnotationsOnWrongType extends UseCaseInput{
        @NotEmptyInputField
        private Integer someNumber;
    }

    @Getter
    @Setter
    public static class SomeInputWithValidInnerPropertiesOnWrongType extends UseCaseInput{
        @ValidInnerPropertiesInputField
        private Integer someNumber;
    }

    @Getter
    @Setter
    public static class SomeInputWithValidInnerPropertiesOnListOfNonUseCaseItems extends UseCaseInput{
        @ValidInnerPropertiesInputField
        private List<Integer> someNumbers;
    }

    @Getter(AccessLevel.PRIVATE)
    @Setter
    public static class SomeInputWithNoPublicGetterForItsProperties extends UseCaseInput{

        @NotNullInputField
        private String someField;

    }

    @Getter
    @Setter
    public static class SomeInputWithRBACIdentifiers extends UseCaseInput{

        @ResourceIdentifier
        private String someId;

        @ResourceOwnerIdentifier
        private String someOtherId;

    }

    @RequiredArgsConstructor
    public static class SomeInputWithAProblematicGetter extends UseCaseInput{

        @ResourceIdentifier
        @NotNullInputField
        private final Long someField;

        @ResourceOwnerIdentifier
        @NotNullInputField
        private final Long anotherField;

        public Long getSomeField(){
            throw new RuntimeException("ouch!!");
        }

        public Long getAnotherField(){
            throw new RuntimeException("ouch!!");
        }

    }

}
