package com.cae.autodoc;

import com.cae.use_cases.UseCase;
import com.cae.autoauth.annotations.ScopeBasedProtection;
import com.cae.use_cases.ConsumerUseCase;
import com.cae.use_cases.FunctionUseCase;
import com.cae.use_cases.RunnableUseCase;
import com.cae.use_cases.SupplierUseCase;
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
                .useCaseSourceCode(UseCaseCodeRetriever.retrieveCodeFor(
                        implementationClass.getPackageName(),
                        implementationClass.getSimpleName(),
                        kotlin
                ))
                .build();
    }

    private static Class<?> getDeclarationClassOf(Class<? extends UseCase> useCaseClass) {
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

    private static Boolean handleProtectionStatus(Class<? extends UseCase> useCaseClass, Class<?> rootUseCaseClass) {
        return useCaseClass.isAnnotationPresent(ScopeBasedProtection.class) || rootUseCaseClass.isAnnotationPresent(ScopeBasedProtection.class);
    }

    private static List<String> handleScopes(Class<? extends UseCase> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(ScopeBasedProtection.class))
            return List.of(useCaseClass.getAnnotation(ScopeBasedProtection.class).scope());
        if (rootUseCaseClass.isAnnotationPresent(ScopeBasedProtection.class))
            return List.of(rootUseCaseClass.getAnnotation(ScopeBasedProtection.class).scope());
        return new ArrayList<>();
    }

    private String useCaseDeclaration;
    private String useCaseImplementation;
    private String useCaseDeclarationLocation;
    private String useCaseImplementationLocation;
    private List<IOContractDocumentation> ioContract;
    private String description;
    private String useCaseSourceCode;
    private Boolean isProtected;
    private List<String> scopes;

}
