package com.cae.framework.use_cases.boundaries;

import com.cae.framework.autofeatures.autoauth.AutoauthModes;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = ElementType.TYPE)
public @interface Edge {

    AutoauthModes autoauth() default AutoauthModes.NOT_EXPLICITLY_INFORMED;
    String[] scopes() default {};
    String actionId() default "";

}
