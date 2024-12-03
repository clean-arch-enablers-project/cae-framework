package com.cae.use_cases.autodoc;

import com.cae.loggers.native_io_extraction_mode.json.sensitive.Sensitive;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.*;

import java.lang.reflect.Field;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
@AllArgsConstructor
@NoArgsConstructor
public class IOFieldDocumentation {

    public static IOFieldDocumentation of(Field field){
        return IOFieldDocumentation.builder()
                .fieldName(field.getName())
                .fieldType(field.getType().getSimpleName())
                .isNotNull(field.isAnnotationPresent(NotNullInputField.class))
                .isNotBlank(field.isAnnotationPresent(NotBlankInputField.class))
                .isNotEmpty(field.isAnnotationPresent(NotEmptyInputField.class))
                .isSensitive(field.isAnnotationPresent(Sensitive.class))
                .build();
    }

    private String fieldName;
    private String fieldType;
    private Boolean isNotNull;
    private Boolean isNotBlank;
    private Boolean isNotEmpty;
    private Boolean isSensitive;

}
