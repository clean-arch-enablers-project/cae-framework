package com.cae.reflection;

import lombok.Builder;
import lombok.Getter;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

@Builder
@Getter
public class FieldAndGetter{
    private Field field;
    private Method getter;
}
