package com.cae.framework.autofeatures.autodoc.components;

import com.cae.framework.autofeatures.autodoc.AutodocNoteExtractor;
import com.cae.framework.autofeatures.autodoc.AutodocSourceCodeRetriever;
import com.cae.framework.entities.BusinessEntity;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.*;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Builder
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Setter
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
        var annotation = Optional.ofNullable(entityClass.getAnnotation(BusinessEntity.class))
                .orElseThrow(() -> new InternalMappedException(
                    "Couldn't instantiate BusinessEntityDocumentation outta '" +entityClass.getSimpleName() + "'",
                    "It must be annotated with @BusinessEntity"
                ));
        return BusinessEntityDocumentation.builder()
                .ontology(Arrays.stream(annotation.ontology()).map(Enum::name).collect(Collectors.toList()))
                .canonicalName(annotation.canonicalName().length == 0? List.of(entityClass.getSimpleName()) : List.of(annotation.canonicalName()))
                .groupsCanonicalEntities(annotation.groupsCanonicalEntities())
                .name(entityClass.getSimpleName())
                .properties(properties)
                .behaviors(allBehaviors)
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        entityClass.getPackageName(),
                        entityClass.getSimpleName(),
                        java
                ))
                .groupsCanonicalEntities(annotation.groupsCanonicalEntities())
                .note(AutodocNoteExtractor.getNoteFrom(entityClass))
                .build();
    }

    private String name;
    private List<ClassProperty> properties;
    private List<ClassBehavior> behaviors;
    private String sourceCode;
    private String note;
    private List<String> ontology;
    private List<String> canonicalName;
    private Boolean groupsCanonicalEntities;


    @Override
    public void cleanSourceCode() {
        this.sourceCode = null;
    }
}
