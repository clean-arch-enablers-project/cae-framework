package com.cae.autofeatures.autocache;

import com.cae.autofeatures.autocache.annotations.AutocacheKey;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Optional;
import java.util.stream.Stream;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AutocacheKeyExtractor {

    public static String runOn(Object object){
        var objectClass = object.getClass();
        return getAnnotatedFieldOutta(objectClass)
                .map(field -> getValueOuttaField(field, object))
                .orElseGet(() -> getFromMethod(object, objectClass));
    }

    private static Optional<Field> getAnnotatedFieldOutta(Class<?> objectClass) {
        return Stream.of(objectClass.getDeclaredFields())
                .filter(field -> field.isAnnotationPresent(AutocacheKey.class))
                .findFirst();
    }

    private static String getValueOuttaField(Field field, Object object) {
        field.setAccessible(true);
        try {
            if (field.getType().equals(String.class))
                return (String) field.get(object);
            throw new InternalMappedException(
                "@AutocacheKey must be a String",
                "The field '" + field.getName() + "' is not String"
            );
        } catch (IllegalAccessException e) {
            throw new InternalMappedException(
                "Something went wrong while trying to get @AutocacheKey value",
                "The problem was during attempt to get value from the '" + field.getName() + "' field",
                e
            );
        }
    }

    private static String getFromMethod(Object object, Class<?> objectClass) {
        return getAnnotatedMethodOutta(objectClass)
                .map(method -> getValueOuttaMethod(method, object))
                .orElseThrow(() -> new InternalMappedException(
                    "Couldn't find any @AutocacheKey",
                    "The '" + objectClass.getSimpleName() + "' had no fields nor methods annotated with @AutocacheKey"
                ));
    }

    private static Optional<Method> getAnnotatedMethodOutta(Class<?> objectClass) {
        return Stream.of(objectClass.getDeclaredMethods())
                .filter(method -> method.isAnnotationPresent(AutocacheKey.class))
                .findFirst();
    }

    private static String getValueOuttaMethod(Method method, Object object) {
        method.setAccessible(true);
        try{
            if (method.getReturnType().equals(String.class))
                if (method.getParameters().length == 0)
                    return (String) method.invoke(object);
                else
                    throw new InternalMappedException(
                        "Couldn't invoke '" + method.getName() + "'",
                        "The @AutocacheKey annotated method must not have parameters"
                    );
            throw new InternalMappedException(
                "@AutocacheKey must be a String",
                "The method '" + method.getName() + "' does not return a String"
            );
        } catch (IllegalAccessException | InvocationTargetException e){
            throw new InternalMappedException(
                    "Something went wrong while trying to get @AutocacheKey value",
                    "The problem was during attempt to get value from the '" + method.getName() + "' method",
                    e
            );
        }
    }

}
