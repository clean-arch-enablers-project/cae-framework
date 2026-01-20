package com.cae.framework.use_cases.metadata;

import com.cae.framework.autofeatures.autoauth.AutoauthModes;
import com.cae.framework.autofeatures.autocache.Cacheable;
import com.cae.framework.autofeatures.autocache.annotations.Autocache;
import com.cae.framework.autofeatures.autocache.metadata.AutocacheMetadata;
import com.cae.framework.use_cases.UseCase;
import com.cae.framework.use_cases.boundaries.Edge;
import com.cae.framework.use_cases.boundaries.Internal;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;

import java.util.Optional;

@Getter
public class UseCaseMetadata {

    private final String id;
    private final String name;
    private final boolean isProtected;
    private final String[] scopes;
    private final boolean isRoleProtectionEnabled;
    private final AutocacheMetadata autocacheMetadata;


    public static <U extends UseCase> UseCaseMetadata of(U useCase) {
        var type = useCase.getClass();
        UseCaseMetadata.findOutWhetherOrNotCached(type);
        return UseCaseMetadata.validateBoundaryAndAutoauth(type, type.getSimpleName());
    }

    private static UseCaseMetadata validateBoundaryAndAutoauth(Class<?> useCaseClass, String name) {
        var foundNothing = true;
        var id = "";
        var requiredScopes = new String[]{};
        var rbac = false;
        var annotatedWithEdge = useCaseClass.isAnnotationPresent(Edge.class);
        var annotatedWithInternal = useCaseClass.isAnnotationPresent(Internal.class);
        if (annotatedWithEdge && annotatedWithInternal)
            throw new InternalMappedException(
                "Couldn't instantiate '" + useCaseClass.getSimpleName() + "'",
                "Its type is annotated with both @Edge and @Internal. Pick one of them."
            );
        if (annotatedWithEdge){
            foundNothing = false;
            var annotation = useCaseClass.getAnnotation(Edge.class);
            if (annotation.scopes().length> 0 && !(annotation.actionId().isBlank()))
                throw new InternalMappedException(
                    "Couldn't instantiate '" + useCaseClass.getSimpleName() + "'",
                    "Its type is annotated with @Edge and had both 'scopes' and 'actionId' setups. Pick one of them."
                );
            if (annotation.scopes().length > 0)
                requiredScopes = annotation.scopes();
            if (!annotation.actionId().isBlank()){
                rbac = true;
                id = annotation.actionId();
            }
            var autoauthMode = annotation.autoauth();
            if (autoauthMode == AutoauthModes.SCOPES){
                if (annotation.scopes().length == 0)
                    throw new InternalMappedException(
                        "Unable to instantiate '" + useCaseClass.getSimpleName() + "'",
                        "Its type is annotated with @Edge(autoauth = AutoauthModes.SCOPES) but had no scopes. " +
                        "Either provide some or remove the autoauth modification."
                    );
                requiredScopes = annotation.scopes();
            }
            else if (autoauthMode == AutoauthModes.RBAC){
                if (annotation.scopes().length > 0)
                    throw new InternalMappedException(
                        "Couldn't instantiate '" + useCaseClass.getSimpleName() + "'",
                        "Its type is annotated with @Edge(autoauth = AutoauthModes.RBAC) but declares scopes. " +
                        "Either remove the scopes or set the autoauth as AutoauthModes.SCOPES (or just omit the autoauth modification)"
                    );
                if (annotation.actionId().isBlank())
                    throw new InternalMappedException(
                        "Could not instantiate '" + useCaseClass.getSimpleName() + "'",
                        "Its type is annotated with @Edge(autoauth = AutoauthModes.RBAC) but declares no ID. " +
                        "Make sure an ID is provided to have your UseCase represented as an action for the RBAC evaluation."
                    );
                rbac = true;
                id = annotation.actionId();
            }
        }
        if (annotatedWithInternal){
            foundNothing = false;
            var annotation = useCaseClass.getAnnotation(Internal.class);
            requiredScopes = Optional.ofNullable(annotation.scopes()).orElse(new String[]{});
        }
        if (foundNothing && useCaseClass != UseCase.class)
            return UseCaseMetadata.validateBoundaryAndAutoauth(useCaseClass.getSuperclass(), name);
        return new UseCaseMetadata(
            useCaseClass,
            id,
            name,
            (requiredScopes.length > 0 || rbac),
            requiredScopes,
            rbac);
    }

    private static boolean findOutWhetherOrNotCached(Class<?> useCaseType) {
        var isAnnotated = useCaseType.isAnnotationPresent(Autocache.class);
        if (isAnnotated && Cacheable.class.isAssignableFrom(useCaseType))
            return true;
        else if (isAnnotated && !(Cacheable.class.isAssignableFrom(useCaseType)))
            throw new InternalMappedException(
                "Autocache annotation used on wrong type of UseCase",
                "Autocache is only allowed on use case instances that extend the Cacheable interface"
            );
        if (useCaseType == UseCase.class)
            return false;
        return UseCaseMetadata.findOutWhetherOrNotCached(useCaseType.getSuperclass());
    }

    protected UseCaseMetadata(
            Class<?> useCaseType,
            String id,
            String name,
            boolean isProtected,
            String[] scopes,
            boolean isRoleProtectionEnabled) {
        this.id = id;
        this.name = name;
        this.isProtected = isProtected;
        this.scopes = scopes;
        this.isRoleProtectionEnabled = isRoleProtectionEnabled;
        this.autocacheMetadata = AutocacheMetadata.of(this.getAutocacheAnnotationOutta(useCaseType));
    }

    private Autocache getAutocacheAnnotationOutta(Class<?> useCaseType) {
        var annotation = useCaseType.getAnnotation(Autocache.class);
        if (annotation != null)
            return annotation;
        if (useCaseType != UseCase.class)
            return this.getAutocacheAnnotationOutta(useCaseType.getSuperclass());
        return null;
    }

    public boolean usesAutocache(){
        return this.autocacheMetadata != null;
    }

}
