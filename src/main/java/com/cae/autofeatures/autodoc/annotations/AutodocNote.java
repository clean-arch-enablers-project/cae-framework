package com.cae.autofeatures.autodoc.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = {
    ElementType.FIELD,
    ElementType.METHOD,
    ElementType.PARAMETER,
    ElementType.TYPE,
    ElementType.CONSTRUCTOR,
    ElementType.ANNOTATION_TYPE
})
public @interface AutodocNote {

    String value();

}
