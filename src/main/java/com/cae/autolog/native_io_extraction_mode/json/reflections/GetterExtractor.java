package com.cae.autolog.native_io_extraction_mode.json.reflections;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetterExtractor {

    public static List<Method> executeOn(Object levelZero){
        var levelZeroClass = levelZero.getClass();
        var methods = levelZeroClass.isEnum()? levelZeroClass.getDeclaredMethods() : levelZeroClass.getMethods();
        return Arrays.stream(methods)
                .filter(method -> method.getName().startsWith("get") && !method.getName().equals("getClass"))
                .collect(Collectors.toList());
    }

}
