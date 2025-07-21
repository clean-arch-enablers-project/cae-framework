package com.cae.autofeatures.autodoc.components;

import com.cae.autofeatures.autodoc.AutodocNoteExtractor;
import com.cae.autofeatures.autodoc.AutodocSourceCodeRetriever;
import lombok.Builder;
import lombok.Getter;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Builder
@Getter
public class ArbitrarySubjectDocumentation {

    public static ArbitrarySubjectDocumentation of(Class<?> artifactClass, boolean kotlin){
        var properties = Stream.of(artifactClass.getDeclaredFields())
                .map(ClassProperty::of)
                .collect(Collectors.toList());
        var allBehaviors = Stream.of(artifactClass.getDeclaredMethods())
                .map(method -> ClassBehavior.of(method, properties))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
        return ArbitrarySubjectDocumentation.builder()
                .name(artifactClass.getSimpleName())
                .properties(properties)
                .behaviors(allBehaviors)
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        artifactClass.getPackageName(),
                        artifactClass.getSimpleName(),
                        kotlin
                ))
                .note(AutodocNoteExtractor.getNoteFrom(artifactClass))
                .build();
    }

    private final String name;
    private final List<ClassProperty> properties;
    private final List<ClassBehavior> behaviors;
    private final String sourceCode;
    private final String note;

}
