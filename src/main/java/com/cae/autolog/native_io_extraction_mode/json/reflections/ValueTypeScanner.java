package com.cae.loggers.native_io_extraction_mode.json.reflections;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.Collection;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ValueTypeScanner {

    public static boolean isSimpleValue(Object value){
        return (value.getClass().isPrimitive() ||
                value.getClass().getPackageName().startsWith("java.") ||
                value.getClass().getPackageName().startsWith("javax.") ||
                value.getClass().getPackageName().startsWith("com.sun.") ||
                value.getClass().getPackageName().startsWith("com.oracle.")) &&
                !(value instanceof Collection);
    }

}
