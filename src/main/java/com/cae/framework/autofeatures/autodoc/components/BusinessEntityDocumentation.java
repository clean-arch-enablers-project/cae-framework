package com.cae.framework.autofeatures.autodoc.components;

import com.cae.framework.autofeatures.autodoc.AutodocNoteExtractor;
import com.cae.framework.autofeatures.autodoc.AutodocSourceCodeRetriever;
import lombok.Builder;
import lombok.Getter;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Builder
@Getter
public class BusinessEntityDocumentation implements Documentation {

    public static BusinessEntityDocumentation of(Class<?> entityClass, boolean java){
        var properties = Stream.of(entityClass.getDeclaredFields())
                .map(ClassProperty::of)
                .collect(Collectors.toList());
        var allBehaviors = Stream.of(entityClass.getDeclaredMethods())
                .map(method -> ClassBehavior.of(method, properties))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
        return BusinessEntityDocumentation.builder()
                .name(entityClass.getSimpleName())
                .properties(properties)
                .behaviors(allBehaviors)
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        entityClass.getPackageName(),
                        entityClass.getSimpleName(),
                        java
                ))
                .note(AutodocNoteExtractor.getNoteFrom(entityClass))
                .build();
    }

    private final String name;
    private final List<ClassProperty> properties;
    private final List<ClassBehavior> behaviors;
    private String sourceCode;
    private final String note;


    @Override
    public void cleanSourceCode() {
        this.sourceCode = null;
    }
}
