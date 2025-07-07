package com.cae.use_cases.io;

import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import com.cae.use_cases.io.annotations.ValidInnerPropertiesInputField;
import com.cae.use_cases.io.exceptions.*;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

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

}
