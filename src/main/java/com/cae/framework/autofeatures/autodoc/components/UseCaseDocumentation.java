package com.cae.framework.autofeatures.autodoc.components;

import com.cae.framework.autofeatures.autoauth.AutoauthModes;
import com.cae.framework.use_cases.boundaries.Edge;
import com.cae.framework.use_cases.boundaries.Internal;
import com.cae.framework.autofeatures.autodoc.AutodocNoteExtractor;
import com.cae.framework.autofeatures.autodoc.AutodocSourceCodeRetriever;
import com.cae.framework.use_cases.ConsumerUseCase;
import com.cae.framework.use_cases.FunctionUseCase;
import com.cae.framework.use_cases.RunnableUseCase;
import com.cae.framework.use_cases.SupplierUseCase;
import lombok.*;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
@AllArgsConstructor
@NoArgsConstructor
public class UseCaseDocumentation {

    public static UseCaseDocumentation of(Class<?> implementationClass, boolean kotlin){
        var declarationClass = getDeclarationClassOf(implementationClass);
        var properties = Stream.of(implementationClass.getDeclaredFields())
                .map(ClassProperty::of)
                .collect(Collectors.toList());
        var allBehaviors = Stream.of(implementationClass.getDeclaredMethods())
                .map(method -> ClassBehavior.of(method, properties))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
        return UseCaseDocumentation.builder()
                .declaration(declarationClass.getSimpleName())
                .declarationPackage(declarationClass.getPackageName())
                .implementation(implementationClass.getSimpleName())
                .implementationPackage(implementationClass.getPackageName())
                .ioContract(handleIOContractFrom(declarationClass))
                .isProtected(handleProtectionStatus(implementationClass, declarationClass))
                .scopes(handleScopes(implementationClass, declarationClass))
                .actionId(handleActionId(implementationClass, declarationClass))
                .properties(properties)
                .behaviors(allBehaviors)
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        implementationClass.getPackageName(),
                        implementationClass.getSimpleName(),
                        kotlin
                ))
                .note(AutodocNoteExtractor.getNoteFrom(implementationClass))
                .build();
    }

    private static Class<?> getDeclarationClassOf(Class<?> useCaseClass) {
        var currentSuperClass = useCaseClass.getSuperclass();
        if (currentSuperClass == FunctionUseCase.class || currentSuperClass == ConsumerUseCase.class || currentSuperClass == SupplierUseCase.class || currentSuperClass == RunnableUseCase.class)
            return useCaseClass;
        return currentSuperClass;
    }

    private static List<IOContractDocumentation> handleIOContractFrom(Class<?> rootUseCaseClass) {
        if (rootUseCaseClass.getSuperclass() == RunnableUseCase.class)
            return new ArrayList<>();
        var ioContract = rootUseCaseClass.getGenericSuperclass();
        return Stream.of(((ParameterizedType) ioContract).getActualTypeArguments())
                .map(IOContractDocumentation::of)
                .collect(Collectors.toList());
    }

    private static Boolean handleProtectionStatus(Class<?> useCaseClass, Class<?> rootUseCaseClass) {
        return (
            (useCaseClass.isAnnotationPresent(Internal.class) &&
            useCaseClass.getAnnotation(Internal.class).scopes() != null &&
            useCaseClass.getAnnotation(Internal.class).scopes().length > 0)
            ||
            (useCaseClass.isAnnotationPresent(Edge.class) &&
            useCaseClass.getAnnotation(Edge.class).scopes() != null &&
            useCaseClass.getAnnotation(Edge.class).scopes().length > 0)
            ||
            (useCaseClass.isAnnotationPresent(Edge.class) &&
            useCaseClass.getAnnotation(Edge.class).autoauth() != null &&
            useCaseClass.getAnnotation(Edge.class).autoauth() == AutoauthModes.RBAC)
        )
        ||
        (
            (rootUseCaseClass.isAnnotationPresent(Internal.class) &&
            rootUseCaseClass.getAnnotation(Internal.class).scopes() != null &&
            rootUseCaseClass.getAnnotation(Internal.class).scopes().length > 0)
            ||
            (rootUseCaseClass.isAnnotationPresent(Edge.class) &&
            rootUseCaseClass.getAnnotation(Edge.class).scopes() != null &&
            rootUseCaseClass.getAnnotation(Edge.class).scopes().length > 0)
            ||
            (rootUseCaseClass.isAnnotationPresent(Edge.class) &&
            rootUseCaseClass.getAnnotation(Edge.class).autoauth() != null &&
            rootUseCaseClass.getAnnotation(Edge.class).autoauth() == AutoauthModes.RBAC)
        );
    }

    private static List<String> handleScopes(Class<?> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(Internal.class))
            return Optional.ofNullable(useCaseClass.getAnnotation(Internal.class).scopes())
                    .map(List::of)
                    .orElse(new ArrayList<>());
        if (rootUseCaseClass.isAnnotationPresent(Internal.class))
            return Optional.ofNullable(rootUseCaseClass.getAnnotation(Internal.class).scopes())
                    .map(List::of)
                    .orElse(new ArrayList<>());
        if (useCaseClass.isAnnotationPresent(Edge.class))
            return Optional.ofNullable(useCaseClass.getAnnotation(Edge.class).scopes())
                    .map(List::of)
                    .orElse(new ArrayList<>());
        if (rootUseCaseClass.isAnnotationPresent(Edge.class))
            return Optional.ofNullable(rootUseCaseClass.getAnnotation(Edge.class).scopes())
                    .map(List::of)
                    .orElse(new ArrayList<>());
        return new ArrayList<>();
    }

    private static String handleActionId(Class<?> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(Edge.class))
            return useCaseClass.getAnnotation(Edge.class).actionId();
        if (rootUseCaseClass.isAnnotationPresent(Edge.class))
            return rootUseCaseClass.getAnnotation(Edge.class).actionId();
        return null;
    }

    private String declaration;
    private String implementation;
    private String declarationPackage;
    private String implementationPackage;
    private List<IOContractDocumentation> ioContract;
    private String sourceCode;
    private Boolean isProtected;
    private List<String> scopes;
    private String actionId;
    private String note;
    private List<ClassProperty> properties;
    private List<ClassBehavior> behaviors;

}
