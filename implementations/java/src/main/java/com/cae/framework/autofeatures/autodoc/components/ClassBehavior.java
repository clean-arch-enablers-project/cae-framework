package com.cae.framework.autofeatures.autodoc.components;

import com.cae.framework.autofeatures.autodoc.AutodocNoteExtractor;
import lombok.*;

import java.lang.reflect.*;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Builder
@Getter
@AllArgsConstructor
@Setter
@NoArgsConstructor
public class ClassBehavior {

    public static Optional<ClassBehavior> of(Method method, List<ClassProperty> properties){
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
                .map(ClassBehaviorParameter::of)
                .collect(Collectors.toList());
        var returnTypeIsCollection = Collection.class.isAssignableFrom(method.getReturnType());
        var behavior = ClassBehavior.builder()
                .name(methodName)
                .isPublic(Modifier.isPublic(modifiers))
                .isStatic(Modifier.isStatic(modifiers))
                .parameters(parameters)
                .returns(method.getReturnType().getSimpleName())
                .returnInnerItemsType(returnTypeIsCollection? extractInnerItemsType(method.getGenericReturnType()) : null)
                .note(AutodocNoteExtractor.getNoteFrom(method))
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

    private String name;
    private Boolean isPublic;
    private Boolean isStatic;
    private List<ClassBehaviorParameter> parameters;
    private String returns;
    private String returnInnerItemsType;
    private String note;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class ClassBehaviorParameter{

        public static ClassBehaviorParameter of(Parameter parameter){
            var isCollection = Collection.class.isAssignableFrom(parameter.getType());
            return new ClassBehaviorParameter(
                    parameter.getName(),
                    parameter.getType().getSimpleName(),
                    isCollection? extractInnerItemsType(parameter.getParameterizedType()) : null,
                    AutodocNoteExtractor.getNoteFrom(parameter)
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

        private String name;
        private String type;
        private String innerItemsType;
        private String note;
    }

}
