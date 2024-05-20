package com.cae.use_cases.autodocumentation;

import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.lang.reflect.Field;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
public class IOFieldDocumentation {

    public static IOFieldDocumentation of(Field field){
        return IOFieldDocumentation.builder()
                .fieldName(field.getName())
                .fieldType(field.getType().getSimpleName())
                .isNotNull(field.isAnnotationPresent(NotNullInputField.class))
                .isNotBlank(field.isAnnotationPresent(NotBlankInputField.class))
                .isNotEmpty(field.isAnnotationPresent(NotEmptyInputField.class))
                .build();
    }

    private String fieldName;
    private String fieldType;
    private Boolean isNotNull;
    private Boolean isNotBlank;
    private Boolean isNotEmpty;

}
