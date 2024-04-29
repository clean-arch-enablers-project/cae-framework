package com.cae.loggers.native_io_extraction_mode.json.reflections;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetterInvoker {

    public static Object execute(Method getter, Object object){
        try {
            return getter.invoke(object);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new InternalMappedException("Something went wrong trying to invoke a getter method", "More details: " + e);
        }
    }

}
