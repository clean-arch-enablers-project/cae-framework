package com.cae.use_cases.io;

import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import com.cae.use_cases.io.annotations.ValidInnerPropertiesInputField;
import br.com.stockio.use_cases.io.exceptions.*;
import com.cae.use_cases.io.exceptions.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Optional;

/**
 * Any use case that accepts input will require the type of it to be an
 * inheritor of this class. Once you declare your use case input as an
 * inheritor of this class, when the use case gets executed it will be able
 * to call the validateProperties() method of your use case input object
 * under the hood and check if fields are compliant to their
 * specifications, so you don't have to implement those kind of validations
 * yourself.
 */
public class UseCaseInput {

    public void validateProperties(){
        try {
            var fields = this.getClass().getDeclaredFields();
            for (var field : fields) {
                var getterMethod = this.getGetterMethodOf(field);
                this.handleNotBlankAnnotation(field, getterMethod);
                this.handleNotEmptyAnnotation(field, getterMethod);
                this.handleValidInnerPropertiesAnnotation(field, getterMethod);
                this.handleNotNullAnnotation(field, getterMethod);
            }
            this.validatePropertiesArbitrarily();
        } catch (MappedException mappedException){
            throw mappedException;
        } catch (Exception e) {
            throw new InternalMappedException("Something went wrong while trying to validate properties of use case input object.",  "More details: " + e);
        }
    }

    /**
     * Meant for client code to override if necessary to implement
     * arbitrarily validation rules in the instance
     */
    protected void validatePropertiesArbitrarily(){}

    private Method getGetterMethodOf(Field field) {
        return Arrays.stream(this.getClass().getMethods())
                .filter(method -> method.getName().equalsIgnoreCase("get".concat(field.getName())))
                .findFirst()
                .orElseThrow(() -> new GetterMethodNotFoundException(field));
    }

    private void handleNotBlankAnnotation(Field field, Method getterMethod) throws IllegalAccessException, InvocationTargetException {
        if (field.isAnnotationPresent(NotBlankInputField.class)){
            Optional.ofNullable(getterMethod.invoke(this)).ifPresent(value -> {
                if (value instanceof String){
                    if (((String) value).isBlank())
                        throw new BlankFieldException(field.getName());
                } else
                    throw new NotBlankAnnotationOnWrongTypeException(field.getName());
            });
        }
    }

    private void handleNotEmptyAnnotation(Field field, Method getterMethod) throws IllegalAccessException, InvocationTargetException {
        if (field.isAnnotationPresent(NotEmptyInputField.class)){
            Optional.ofNullable(getterMethod.invoke(this)).ifPresent(value -> {
                if (value instanceof String){
                    if (((String) value).isEmpty())
                        throw new EmptyFieldException(field.getName());
                } else
                    throw new NotEmptyAnnotationOnWrongTypeException(field.getName());
            });
        }
    }

    private void handleValidInnerPropertiesAnnotation(Field field, Method getterMethod) throws IllegalAccessException, InvocationTargetException {
        if (field.isAnnotationPresent(ValidInnerPropertiesInputField.class)){
            Optional.ofNullable(getterMethod.invoke(this)).ifPresent(value -> {
                if (value instanceof UseCaseInput)
                    ((UseCaseInput) value).validateProperties();
                else
                    throw new ValidInnerPropertiesAnnotationOnWrongTypeException(field.getName());
            });
        }
    }

    private void handleNotNullAnnotation(Field field, Method getterMethod) throws IllegalAccessException, InvocationTargetException {
        if (field.isAnnotationPresent(NotNullInputField.class)){
            var value = getterMethod.invoke(this);
            this.checkIfNotNull(value, field);
        }
    }

    private void checkIfNotNull(Object value, Field field){
        if (Optional.ofNullable(value).isEmpty())
            throw new NullFieldException(field.getName());
    }

    public static class GetterMethodNotFoundException extends InternalMappedException {
        public GetterMethodNotFoundException(Field field) {
            super("Getter method not found for one of the fields.", "More details: the field '" + field.getName() + "' has no getter method defined for it. Please define one method for this purpose.");
        }
    }

}
