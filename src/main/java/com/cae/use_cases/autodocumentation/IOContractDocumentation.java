package com.cae.use_cases.autodocumentation;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.lang.reflect.Type;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
public class IOContractDocumentation {

    public static IOContractDocumentation of(Type ioType){
        var typeAsClass = (Class<?>) ioType;
        return IOContractDocumentation.builder()
                .className(typeAsClass.getSimpleName())
                .classFields(handleFieldsFrom(typeAsClass))
                .build();
    }

    private static List<IOFieldDocumentation> handleFieldsFrom(Class<?> typeAsClass) {
        return Stream.of(typeAsClass.getDeclaredFields())
                .map(IOFieldDocumentation::of)
                .collect(Collectors.toList());
    }

    private String className;
    private List<IOFieldDocumentation> classFields;

}
