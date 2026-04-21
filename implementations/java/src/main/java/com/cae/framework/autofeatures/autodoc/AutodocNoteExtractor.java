package com.cae.framework.autofeatures.autodoc;

import com.cae.framework.autofeatures.autodoc.annotations.AutodocNote;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AutodocNoteExtractor {

    public static String getNoteFrom(Field field){
        if (field.isAnnotationPresent(AutodocNote.class))
            return field.getAnnotation(AutodocNote.class).value();
        return null;
    }

    public static String getNoteFrom(Method method){
        if (method.isAnnotationPresent(AutodocNote.class))
            return method.getAnnotation(AutodocNote.class).value();
        return null;
    }

    public static String getNoteFrom(Parameter method){
        if (method.isAnnotationPresent(AutodocNote.class))
            return method.getAnnotation(AutodocNote.class).value();
        return null;
    }

    public static String getNoteFrom(Class<?> clazz){
        if (clazz.isAnnotationPresent(AutodocNote.class))
            return clazz.getAnnotation(AutodocNote.class).value();
        return null;
    }

}
