package com.cae.autolog.native_io_extraction_mode.json.reflections;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Optional;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldRetriever {

    public static Optional<Field> getField(String fieldName, Object object){
        var classWhereFieldIsDeclared = Optional.ofNullable(FieldRetriever.findRightClassFrom(object.getClass(), fieldName));
        return classWhereFieldIsDeclared.map(foundClass -> FieldRetriever.retrieveField(foundClass, fieldName));
    }

    private static Class<?> findRightClassFrom(Class<?> clazz, String fieldName) {
        if (clazz == Object.class)
            return null;
        var fields = clazz.getDeclaredFields();
        if (Arrays.stream(fields).anyMatch(field -> field.getName().equals(fieldName)))
            return clazz;
        return FieldRetriever.findRightClassFrom(clazz.getSuperclass(), fieldName);
    }

    private static Field retrieveField(Class<?> foundClass, String fieldName){
        try {
            return foundClass.getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to retrieve the field '" + fieldName + "' in '" + foundClass.getSimpleName() + "'",
                    "We found the class in which the field should be located, but at the retrievement attempt something went unexpectedly wrong. See the exception: " + e
            );
        }
    }

}
