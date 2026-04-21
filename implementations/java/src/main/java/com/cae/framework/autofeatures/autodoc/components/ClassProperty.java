package com.cae.framework.autofeatures.autodoc.components;

import com.cae.framework.autofeatures.autodoc.AutodocNoteExtractor;
import lombok.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.util.Collection;

@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ClassProperty{

    public static ClassProperty of(Field field){
        var modifiers = field.getModifiers();
        var isCollection = Collection.class.isAssignableFrom(field.getType());
        return ClassProperty.builder()
                .name(field.getName())
                .isCollection(isCollection)
                .innerItemsType(isCollection? extractInnerItemsType(field) : null)
                .type(field.getType().getSimpleName())
                .isPublic(Modifier.isPublic(modifiers))
                .isStatic(Modifier.isStatic(modifiers))
                .note(AutodocNoteExtractor.getNoteFrom(field))
                .build();
    }

    private static String extractInnerItemsType(Field field) {
        var genericType = field.getGenericType();
        if (genericType instanceof ParameterizedType){
            var typeArgs = ((ParameterizedType) genericType).getActualTypeArguments();
            if (typeArgs.length > 0 && typeArgs[0] instanceof Class<?>){
                return ((Class<?>) typeArgs[0]).getSimpleName();
            }
        }
        return null;
    }

    private String name;
    private String type;
    private Boolean isStatic;
    private Boolean isPublic;
    private Boolean isCollection;
    private String innerItemsType;
    private String note;
    private Boolean hasGetter;
    private Boolean isGetterPublic;
    private Boolean hasSetter;
    private Boolean isSetterPublic;
}
