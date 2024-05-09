package com.cae.loggers.native_io_extraction_mode.json.reflections;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;
import java.util.Arrays;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldRetriever {

    public static Field getField(String fieldName, Object object){
        try {
            var classWhereFieldIsDeclared = FieldRetriever.findRightClassFrom(object.getClass(), fieldName);
            return classWhereFieldIsDeclared.getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to retrieve the field " + fieldName,
                    "More details: " + e
            );
        }
    }

    private static Class<?> findRightClassFrom(Class<?> clazz, String fieldName) {
        if (clazz == Object.class)
            throw new InternalMappedException("Problem trying to extract I/O data", "Not possible to find the class where the field '" + fieldName + "' was declared");
        var fields = clazz.getDeclaredFields();
        if (Arrays.stream(fields).anyMatch(field -> field.getName().equals(fieldName)))
            return clazz;
        return FieldRetriever.findRightClassFrom(clazz.getSuperclass(), fieldName);
    }

}
