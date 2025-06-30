package com.cae.autofeatures.autodoc.components;

import com.cae.autofeatures.autoauth.annotations.RoleBasedProtection;
import com.cae.autofeatures.autoauth.annotations.ScopeBasedProtection;
import com.cae.autofeatures.autodoc.AutodocSourceCodeRetriever;
import com.cae.use_cases.*;
import lombok.*;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
@AllArgsConstructor
@NoArgsConstructor
public class UseCaseDocumentation {

    public static UseCaseDocumentation of(UseCase useCase, boolean kotlin){
        var implementationClass = useCase.getClass();
        var declarationClass = getDeclarationClassOf(implementationClass);
        return UseCaseDocumentation.builder()
                .useCaseDeclaration(declarationClass.getSimpleName())
                .useCaseDeclarationLocation(declarationClass.getPackageName())
                .useCaseImplementation(implementationClass.getSimpleName())
                .useCaseImplementationLocation(implementationClass.getPackageName())
                .ioContract(handleIOContractFrom(declarationClass))
                .isProtected(handleProtectionStatus(implementationClass, declarationClass))
                .scopes(handleScopes(implementationClass, declarationClass))
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        implementationClass.getPackageName(),
                        implementationClass.getSimpleName(),
                        kotlin
                ))
                .build();
    }

    private static Class<?> getDeclarationClassOf(Class<?> useCaseClass) {
        var currentSuperClass = useCaseClass.getSuperclass();
        if (currentSuperClass == FunctionUseCase.class || currentSuperClass == ConsumerUseCase.class || currentSuperClass == SupplierUseCase.class || currentSuperClass == RunnableUseCase.class)
            return useCaseClass;
        return currentSuperClass;
    }

    public static UseCaseDocumentation of(Class<?> implementationClass, boolean kotlin){
        var declarationClass = getDeclarationClassOf(implementationClass);
        return UseCaseDocumentation.builder()
                .useCaseDeclaration(declarationClass.getSimpleName())
                .useCaseDeclarationLocation(declarationClass.getPackageName())
                .useCaseImplementation(implementationClass.getSimpleName())
                .useCaseImplementationLocation(implementationClass.getPackageName())
                .ioContract(handleIOContractFrom(declarationClass))
                .isProtected(handleProtectionStatus(implementationClass, declarationClass))
                .scopes(handleScopes(implementationClass, declarationClass))
                .actionId(handleActionId(implementationClass, declarationClass))
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        implementationClass.getPackageName(),
                        implementationClass.getSimpleName(),
                        kotlin
                ))
                .build();
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
        return useCaseClass.isAnnotationPresent(ScopeBasedProtection.class) || rootUseCaseClass.isAnnotationPresent(ScopeBasedProtection.class);
    }

    private static List<String> handleScopes(Class<?> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(ScopeBasedProtection.class))
            return List.of(useCaseClass.getAnnotation(ScopeBasedProtection.class).scope());
        if (rootUseCaseClass.isAnnotationPresent(ScopeBasedProtection.class))
            return List.of(rootUseCaseClass.getAnnotation(ScopeBasedProtection.class).scope());
        return new ArrayList<>();
    }

    private static String handleActionId(Class<?> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(RoleBasedProtection.class))
            return useCaseClass.getAnnotation(RoleBasedProtection.class).actionId();
        if (rootUseCaseClass.isAnnotationPresent(RoleBasedProtection.class))
            return rootUseCaseClass.getAnnotation(RoleBasedProtection.class).actionId();
        return null;
    }

    private String useCaseDeclaration;
    private String useCaseImplementation;
    private String useCaseDeclarationLocation;
    private String useCaseImplementationLocation;
    private List<IOContractDocumentation> ioContract;
    private String description;
    private String sourceCode;
    private Boolean isProtected;
    private List<String> scopes;
    private String actionId;

}
