package com.cae.use_cases.metadata;

import com.cae.autofeatures.autoauth.annotations.RoleBasedProtection;
import com.cae.autofeatures.autoauth.annotations.ScopeBasedProtection;
import com.cae.autofeatures.autocache.annotations.Autocache;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseAsAction;
import com.cae.use_cases.UseCaseWithInput;
import lombok.Getter;

@Getter
public class UseCaseMetadata {

    private final String id;
    private final String name;
    private final boolean isProtected;
    private final String[] scope;
    private final boolean roleProtectionEnabled;
    private final boolean isCached;

    public static <U extends UseCase> UseCaseMetadata of(U useCase) {
        var type = useCase.getClass();
        var requiredScope = UseCaseMetadata.getRequiredScopesOutta(type);
        var isRoleProtected = UseCaseMetadata.findOutWhetherOrNotRoleProtected(type);
        var isCached = UseCaseMetadata.findOutWhetherOrNotCached(type);
        return new UseCaseMetadata(
                type,
                (requiredScope.length > 0 || isRoleProtected),
                requiredScope,
                isRoleProtected,
                isCached);
    }

    protected static String[] getRequiredScopesOutta(Class<?> useCaseType) {
        var typeIsAnnotated = useCaseType.isAnnotationPresent(ScopeBasedProtection.class);
        if (typeIsAnnotated)
            return useCaseType.getAnnotation(ScopeBasedProtection.class).scope();
        if (useCaseType == UseCase.class)
            return new String[]{};
        return UseCaseMetadata.getRequiredScopesOutta(useCaseType.getSuperclass());
    }

    protected static boolean findOutWhetherOrNotRoleProtected(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(RoleBasedProtection.class);
        if (isAnnotated)
            return true;
        if (useCaseType == UseCase.class)
            return false;
        return UseCaseMetadata.findOutWhetherOrNotRoleProtected(useCaseType.getSuperclass());
    }

    private static boolean findOutWhetherOrNotCached(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(Autocache.class);
        if (isAnnotated && UseCaseWithInput.class.isAssignableFrom(useCaseType))
            return true;
        else if (isAnnotated && !(UseCaseWithInput.class.isAssignableFrom(useCaseType)))
            throw new InternalMappedException(
                "Autocache annotation used on wrong type of UseCase",
                "Autocache is only allowed on FunctionUseCase and ConsumerUseCase instances (types with input)"
            );
        if (useCaseType == UseCase.class)
            return false;
        return UseCaseMetadata.findOutWhetherOrNotCached(useCaseType.getSuperclass());
    }

    protected <U extends UseCase> UseCaseMetadata(
            Class<U> useCaseType,
            boolean isProtected,
            String[] scope,
            boolean roleProtectionEnabled,
            boolean isCached) {
        this.id = UseCaseMetadata.getIdOutta(useCaseType, roleProtectionEnabled);
        this.name = useCaseType.getSimpleName();
        this.isProtected = isProtected;
        this.scope = scope;
        this.roleProtectionEnabled = roleProtectionEnabled;
        this.isCached = isCached;
    }

    protected static <U extends UseCase> String getIdOutta(
            Class<U> useCaseType,
            Boolean roleProtection) {
        var idFromUseCaseIdAnnotation = UseCaseMetadata.getIdByUseCaseIdAnnotation(useCaseType);
        var idFromRoleBasedProtectedAnnotation = "";
        if (Boolean.TRUE.equals(roleProtection)){
            idFromRoleBasedProtectedAnnotation = UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(useCaseType);
            if (idFromRoleBasedProtectedAnnotation.isBlank() && idFromUseCaseIdAnnotation.isBlank())
                throw new InternalMappedException(
                        "Problem during the extraction of use case ID",
                        "The use case type '" + useCaseType.getSimpleName() + "' doesn't have an ID provided neither by the UseCaseAsAction nor the RoleBasedProtection annotations. Please provide an ID by either one of the annotations."
                );
        }
        return idFromUseCaseIdAnnotation.isBlank()? idFromRoleBasedProtectedAnnotation : idFromUseCaseIdAnnotation;
    }

    protected static String getIdByUseCaseIdAnnotation(Class<?> useCaseType){
        var isAnnotated = useCaseType.isAnnotationPresent(UseCaseAsAction.class);
        if (isAnnotated)
            return useCaseType.getAnnotation(UseCaseAsAction.class).actionId();
        if (useCaseType == UseCase.class)
            return "";
        return UseCaseMetadata.getIdByUseCaseIdAnnotation(useCaseType.getSuperclass());
    }

    protected static String getIdByRoleBasedProtectedUseCaseAnnotation(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(RoleBasedProtection.class);
        if (isAnnotated)
            return useCaseType.getAnnotation(RoleBasedProtection.class).actionId();
        if (useCaseType == UseCase.class)
            return "";
        return UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(useCaseType.getSuperclass());
    }

    protected  <U extends UseCase> String getNameOutta(Class<U> useCaseType) {
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
