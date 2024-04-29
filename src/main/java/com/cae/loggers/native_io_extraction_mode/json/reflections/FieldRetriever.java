package com.cae.loggers.native_io_extraction_mode.json.reflections;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldRetriever {

    public static Field getField(String fieldName, Object object){
        try {
            return object.getClass().getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to retrieve the field " + fieldName,
                    "More details: " + e
            );
        }
    }

}
