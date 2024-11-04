package com.cae.use_cases.metadata;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseId;
import com.cae.use_cases.authorization.annotations.RoleBasedProtectedUseCase;
import com.cae.use_cases.authorization.annotations.ScopeBasedProtectedUseCase;
import lombok.Getter;

/**
 * The metadata of use cases such as name, description and protection status.
 * <p></p>
 * The name of use cases will be set in snake case.
 * <p></p>
 * Their descriptions are for providing a brief but comprehensible
 * overview about their essence.
 * <p></p>
 * The protection status is meant to inform if the use case execution
 * access is open or protected. If protected, it is up to the external layer
 * dispatching the use case to provide the actor info, so the use case processor
 * can run the access validation to decide weather the action will be allowed or not.
 * <p></p>
 * All of this metadata and more can be automatically externalized to a file
 * called cae-docfile.json.
 * It is up to you to decide what to do with the file. For
 * instance, it could be used to expose an overview about the
 * available use cases of the application for governance means.
 */
@Getter
public class UseCaseMetadata {

    private final String id;
    private final String name;
    private final Boolean isProtected;
    private final String[] scope;
    private final Boolean roleProtectionEnabled;

    public static <U extends UseCase> UseCaseMetadata of(U useCase) {
        var type = useCase.getClass();
        var requiredScope = UseCaseMetadata.getRequiredScopesOutta(type);
        var isRoleProtected = UseCaseMetadata.findOutWhetherOrNotRoleProtected(type);
        return new UseCaseMetadata(type, (requiredScope.length > 0 || isRoleProtected), requiredScope, isRoleProtected);
    }

    private static String[] getRequiredScopesOutta(Class<?> useCaseType) {
        var typeIsAnnotated = useCaseType.isAnnotationPresent(ScopeBasedProtectedUseCase.class);
        if (typeIsAnnotated)
            return useCaseType.getAnnotation(ScopeBasedProtectedUseCase.class).scope();
        if (useCaseType == UseCase.class)
            return new String[]{};
        return UseCaseMetadata.getRequiredScopesOutta(useCaseType.getSuperclass());
    }

    private static Boolean findOutWhetherOrNotRoleProtected(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(RoleBasedProtectedUseCase.class);
        if (isAnnotated)
            return true;
        if (useCaseType == UseCase.class)
            return false;
        return UseCaseMetadata.findOutWhetherOrNotRoleProtected(useCaseType.getSuperclass());
    }

    private <U extends UseCase> UseCaseMetadata(
            Class<U> useCaseType,
            Boolean isProtected,
            String[] scope,
            Boolean roleProtectionEnabled) {
        this.id = UseCaseMetadata.getIdOutta(useCaseType, roleProtectionEnabled);
        this.name = getNameOutta(useCaseType);
        this.isProtected = isProtected;
        this.scope = scope;
        this.roleProtectionEnabled = roleProtectionEnabled;
    }

    private static <U extends UseCase> String getIdOutta(
            Class<U> useCaseType,
            Boolean roleProtection) {
        var idFromUseCaseIdAnnotation = UseCaseMetadata.getIdByUseCaseIdAnnotation(useCaseType);
        var idFromRoleBasedProtectedAnnotation = "";
        if (Boolean.TRUE.equals(roleProtection)){
            idFromRoleBasedProtectedAnnotation = UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(useCaseType);
            if (idFromRoleBasedProtectedAnnotation.isBlank() && idFromUseCaseIdAnnotation.isBlank())
                throw new InternalMappedException(
                        "Problem during the extraction of use case ID",
                        "The use case type '" + useCaseType.getSimpleName() + "' doesn't have an ID provided neither by the UseCaseId nor the RoleBasedProtectedUseCase annotations. Please provide an ID by either one of the annotations."
                );
        }
        return idFromUseCaseIdAnnotation.isBlank()? idFromRoleBasedProtectedAnnotation : idFromUseCaseIdAnnotation;
    }

    private static String getIdByUseCaseIdAnnotation(Class<?> useCaseType){
        var isAnnotated = useCaseType.isAnnotationPresent(UseCaseId.class);
        if (isAnnotated)
            return useCaseType.getAnnotation(UseCaseId.class).id();
        if (useCaseType == UseCase.class)
            return "";
        return UseCaseMetadata.getIdByUseCaseIdAnnotation(useCaseType.getSuperclass());
    }

    private static String getIdByRoleBasedProtectedUseCaseAnnotation(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(RoleBasedProtectedUseCase.class);
        if (isAnnotated)
            return useCaseType.getAnnotation(RoleBasedProtectedUseCase.class).useCaseId();
        if (useCaseType == UseCase.class)
            return "";
        return UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(useCaseType.getSuperclass());
    }

    private <U extends UseCase> String getNameOutta(Class<U> useCaseType) {
        var simpleName = useCaseType.getSimpleName();
        var snakeCaseName = new StringBuilder();
        var index = 0;
        for (var character : simpleName.toCharArray()){
            if (Character.isUpperCase(character) && index > 0)
                snakeCaseName.append("_");
            snakeCaseName.append(String.valueOf(character).toLowerCase());
            index ++;
        }
        return snakeCaseName.toString().replace("_use_case", "");
    }

    public Boolean isProtected() {
        return isProtected;
    }

}
