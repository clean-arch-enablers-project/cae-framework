package com.cae.autofeatures.autodoc.components;

import com.cae.autofeatures.autodoc.AutodocSourceCodeRetriever;
import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

import java.lang.reflect.*;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Builder
@Getter
public class EntityDocumentation {

    public static EntityDocumentation of(Class<?> entityClass, boolean kotlin){
        var properties = Stream.of(entityClass.getDeclaredFields())
                .map(EntityProperty::of)
                .collect(Collectors.toList());
        var allBehaviors = Stream.of(entityClass.getDeclaredMethods())
                .map(method -> EntityBehavior.of(method, properties))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toList());
        return EntityDocumentation.builder()
                .name(entityClass.getSimpleName())
                .properties(properties)
                .behaviors(allBehaviors)
                .sourceCode(AutodocSourceCodeRetriever.retrieveCodeFor(
                        entityClass.getPackageName(),
                        entityClass.getSimpleName(),
                        kotlin
                ))
                .build();
    }

    private final String name;
    private final List<EntityProperty> properties;
    private final List<EntityBehavior> behaviors;
    private final String sourceCode;

    @Builder
    @Getter
    @Setter
    public static class EntityProperty{

        public static EntityProperty of(Field field){
            var modifiers = field.getModifiers();
            var isCollection = Collection.class.isAssignableFrom(field.getType());
            return EntityProperty.builder()
                    .name(field.getName())
                    .isCollection(isCollection)
                    .innerItemsType(isCollection? extractInnerItemsType(field) : null)
                    .type(field.getType().getSimpleName())
                    .isPublic(Modifier.isPublic(modifiers))
                    .isStatic(Modifier.isStatic(modifiers))
                    .build();
        }

        private static String extractInnerItemsType(Field field) {
            var genericType = field.getGenericType();
            if (genericType instanceof ParameterizedType){
                var typeArgs = ((ParameterizedType) genericType).getActualTypeArguments();
                if (typeArgs.length > 0 && typeArgs[0] instanceof Class<?>){
                    return ((Class<?>) typeArgs[0]).getSimpleName();
               }
            }
            return null;
        }

        private final String name;
        private final String type;
        private final Boolean isStatic;
        private final Boolean isPublic;
        private final Boolean isCollection;
        private final String innerItemsType;
        private Boolean hasGetter;
        private Boolean isGetterPublic;
        private Boolean hasSetter;
        private Boolean isSetterPublic;
    }

    @Builder
    @Getter
    public static class EntityBehavior {

        public static Optional<EntityBehavior> of(Method method, List<EntityProperty> properties){
            if (method.getName().startsWith("lambda$"))
                return Optional.empty();
            if (method.getDeclaringClass().equals(Object.class))
                return Optional.empty();
            var modifiers = method.getModifiers();
            var methodName = method.getName();
            var withoutGet = methodName.replace("get", "");
            var withoutSet = methodName.replace("set", "");
            var isGetter = false;
            var isSetter = false;
            var counter = 0;
            while ((!isGetter) && (!isSetter) && counter < properties.size()){
                var currentProperty = properties.get(counter);
                if (currentProperty.getName().equalsIgnoreCase(withoutGet)){
                    isGetter = true;
                    currentProperty.setHasGetter(true);
                    currentProperty.setIsGetterPublic(Modifier.isPublic(modifiers));
                }
                else if (currentProperty.getName().equalsIgnoreCase(withoutSet)){
                    isSetter = true;
                    currentProperty.setHasSetter(true);
                    currentProperty.setIsSetterPublic(Modifier.isPublic(modifiers));
                }
                counter += 1;
            }
            if (isGetter || isSetter)
                return Optional.empty();
            var parameters = Stream.of(method.getParameters())
                    .map(EntityBehaviorParameter::of)
                    .collect(Collectors.toList());
            var returnTypeIsCollection = Collection.class.isAssignableFrom(method.getReturnType());
            var behavior = EntityBehavior.builder()
                    .name(methodName)
                    .isPublic(Modifier.isPublic(modifiers))
                    .isStatic(Modifier.isStatic(modifiers))
                    .parameters(parameters)
                    .returns(method.getReturnType().getSimpleName())
                    .returnInnerItemsType(returnTypeIsCollection? extractInnerItemsType(method.getGenericReturnType()) : null)
                    .build();
            return Optional.of(behavior);
        }

        private static String extractInnerItemsType(Type type) {
            if (type instanceof ParameterizedType){
                var typeArgs = ((ParameterizedType) type).getActualTypeArguments();
                if (typeArgs.length > 0 && typeArgs[0] instanceof Class<?>){
                    return ((Class<?>) typeArgs[0]).getSimpleName();
                }
            }
            return null;
        }

        private final String name;
        private final Boolean isPublic;
        private final Boolean isStatic;
        private final List<EntityBehaviorParameter> parameters;
        private final String returns;
        private final String returnInnerItemsType;
    }

    @RequiredArgsConstructor
    @Getter
    public static class EntityBehaviorParameter{

        public static EntityBehaviorParameter of(Parameter parameter){
            var isCollection = Collection.class.isAssignableFrom(parameter.getType());
            return new EntityBehaviorParameter(
                    parameter.getType().getSimpleName(),
                    parameter.getName(),
                    isCollection? extractInnerItemsType(parameter.getParameterizedType()) : null
            );
        }

        private static String extractInnerItemsType(Type type) {
            if (type instanceof ParameterizedType){
                var typeArgs = ((ParameterizedType) type).getActualTypeArguments();
                if (typeArgs.length > 0 && typeArgs[0] instanceof Class<?>){
                    return ((Class<?>) typeArgs[0]).getSimpleName();
                }
            }
            return null;
        }

        private final String name;
        private final String type;
        private final String innerItemsType;
    }

}
