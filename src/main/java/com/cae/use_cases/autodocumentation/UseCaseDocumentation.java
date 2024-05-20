package com.cae.use_cases.autodocumentation;

import com.cae.use_cases.UseCase;
import com.cae.use_cases.authorization.annotations.ProtectedUseCase;
import com.cae.use_cases.specifics.consumers.ConsumerUseCase;
import com.cae.use_cases.specifics.functions.FunctionUseCase;
import com.cae.use_cases.specifics.runnables.RunnableUseCase;
import com.cae.use_cases.specifics.suppliers.SupplierUseCase;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Getter
@Setter
@Builder(access = AccessLevel.PRIVATE)
public class UseCaseDocumentation {

    public static UseCaseDocumentation of(UseCase useCase){
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
        return useCaseClass.isAnnotationPresent(ProtectedUseCase.class) || rootUseCaseClass.isAnnotationPresent(ProtectedUseCase.class);
    }

    private static List<String> handleScopes(Class<? extends UseCase> useCaseClass, Class<?> rootUseCaseClass) {
        if (useCaseClass.isAnnotationPresent(ProtectedUseCase.class))
            return List.of(useCaseClass.getAnnotation(ProtectedUseCase.class).scope());
        if (rootUseCaseClass.isAnnotationPresent(ProtectedUseCase.class))
            return List.of(rootUseCaseClass.getAnnotation(ProtectedUseCase.class).scope());
        return new ArrayList<>();
    }

    private String useCaseDeclaration;
    private String useCaseImplementation;
    private String useCaseDeclarationLocation;
    private String useCaseImplementationLocation;
    private List<IOContractDocumentation> ioContract;
    private String description;
    private Boolean isProtected;
    private List<String> scopes;

}
