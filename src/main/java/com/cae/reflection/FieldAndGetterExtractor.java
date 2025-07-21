package com.cae.reflection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldAndGetterExtractor {

    public static List<FieldAndGetter> runOn(Object object) {
        var fieldsAndGetters = new ArrayList<FieldAndGetter>();
        var fields = object.getClass().getDeclaredFields();
        for (var field : fields){
            var getterMethod = getGetterMethodOf(field, object);
            fieldsAndGetters.add(FieldAndGetter.builder().field(field).getter(getterMethod).build());
        }
        return fieldsAndGetters;
    }

    private static Method getGetterMethodOf(Field field, Object object) {
        return Arrays.stream(object.getClass().getMethods())
                .filter(method -> method.getName().equalsIgnoreCase("get".concat(field.getName())))
                .findFirst()
                .orElseThrow(() -> new GetterMethodNotFoundException(getFullFieldName(field, object)));
    }

    public static String getFullFieldName(Field field, Object object){
        return object.getClass().getSimpleName() + ":" + field.getName();
    }

}
