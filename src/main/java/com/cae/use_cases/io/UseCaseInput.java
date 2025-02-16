package com.cae.use_cases.io;

import com.cae.autolog.AutologProvider;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.autoauth.annotations.ResourceIdentifier;
import com.cae.autoauth.annotations.ResourceOwnerIdentifier;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotEmptyInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import com.cae.use_cases.io.annotations.ValidInnerPropertiesInputField;
import com.cae.use_cases.io.exceptions.*;
import lombok.Builder;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

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

    private List<FieldAndGetter> fieldAndGetterList;

    public void validateProperties(){
        try {
            for (var fieldAndGetter : this.getFieldAndGetterList()) {
                this.handleNotBlankAnnotation(fieldAndGetter);
                this.handleNotEmptyAnnotation(fieldAndGetter);
                this.handleValidInnerPropertiesAnnotation(fieldAndGetter);
                this.handleNotNullAnnotation(fieldAndGetter);
            }
            this.validatePropertiesArbitrarily();
        } catch (MappedException mappedException){
            AutologProvider.SINGLETON.getProvidedInstance().ifPresent(providedLogger -> providedLogger.logError(mappedException.toString()));
            throw mappedException;
        } catch (Exception e) {
            throw new InternalMappedException("Something went wrong while trying to validate properties of use case input object.",  "More details on this: " + e);
        }
    }

    private List<FieldAndGetter> getFieldAndGetterList() {
        return Optional.ofNullable(this.fieldAndGetterList).orElseGet(() -> {
            var fieldsAndGetters = new ArrayList<FieldAndGetter>();
            var fields = this.getClass().getDeclaredFields();
            for (var field : fields){
                var getterMethod = this.getGetterMethodOf(field);
                fieldsAndGetters.add(FieldAndGetter.builder().field(field).getter(getterMethod).build());
            }
            this.fieldAndGetterList = fieldsAndGetters;
            return fieldsAndGetters;
        });
    }

    /**
     * Meant for client code to override if necessary to implement
     * arbitrarily validation rules in the instance
     */
    protected void validatePropertiesArbitrarily(){
        //TODO: Meant for client code to override if necessary to implement arbitrarily validation rules in the instance
    }

    private Method getGetterMethodOf(Field field) {
        return Arrays.stream(this.getClass().getMethods())
                .filter(method -> method.getName().equalsIgnoreCase("get".concat(field.getName())))
                .findFirst()
                .orElseThrow(() -> new GetterMethodNotFoundException(this.getFullFieldName(field)));
    }

    private void handleNotBlankAnnotation(FieldAndGetter fieldAndGetter) throws IllegalAccessException, InvocationTargetException {
        if (fieldAndGetter.field.isAnnotationPresent(NotBlankInputField.class)){
            Optional.ofNullable(fieldAndGetter.getter.invoke(this)).ifPresent(value -> {
                if (value instanceof String){
                    if (((String) value).isBlank())
                        throw new BlankFieldException(this.getFullFieldName(fieldAndGetter.field));
                } else if (value instanceof  Collection){
                    for (var item: (Collection<?>) value)
                        this.validateCollectionItemAsNotBlank(item, fieldAndGetter.field);
                }
                else
                    throw new NotBlankAnnotationOnWrongTypeException(this.getFullFieldName(fieldAndGetter.field));
            });
        }
    }

    private void validateCollectionItemAsNotBlank(Object item, Field field) {
        if (item instanceof String){
            if (((String) item).isBlank())
                throw new BlankFieldException(this.getFullFieldName(field));
        }
        else
            throw new NotBlankAnnotationOnWrongTypeException(this.getFullFieldName(field));
    }

    private void handleNotEmptyAnnotation(FieldAndGetter fieldAndGetter) throws IllegalAccessException, InvocationTargetException {
        if (fieldAndGetter.field.isAnnotationPresent(NotEmptyInputField.class)){
            Optional.ofNullable(fieldAndGetter.getter.invoke(this)).ifPresent(value -> {
                if (value instanceof String){
                    if (((String) value).isEmpty())
                        throw new EmptyFieldException(this.getFullFieldName(fieldAndGetter.field));
                } else if (value instanceof Collection){
                    if (((Collection<?>) value).isEmpty())
                        throw new EmptyFieldException(this.getFullFieldName(fieldAndGetter.field));
                }
                else
                    throw new NotEmptyAnnotationOnWrongTypeException(this.getFullFieldName(fieldAndGetter.field));
            });
        }
    }

    private void handleValidInnerPropertiesAnnotation(FieldAndGetter fieldAndGetter) throws IllegalAccessException, InvocationTargetException {
        if (fieldAndGetter.field.isAnnotationPresent(ValidInnerPropertiesInputField.class)){
            Optional.ofNullable(fieldAndGetter.getter.invoke(this)).ifPresent(value -> {
                if (value instanceof UseCaseInput)
                    ((UseCaseInput) value).validateProperties();
                else if(value instanceof Collection){
                    for (var item : (Collection<?>) value)
                        this.handleCollectionItemValidation(item, fieldAndGetter.field);
                }
                else
                    throw new ValidInnerPropertiesAnnotationOnWrongTypeException(this.getFullFieldName(fieldAndGetter.field));
            });
        }
    }

    private void handleCollectionItemValidation(Object item, Field field){
        if (item instanceof UseCaseInput)
            ((UseCaseInput) item).validateProperties();
        else
            throw new ValidInnerPropertiesAnnotationOnWrongTypeException(this.getFullFieldName(field));
    }

    private void handleNotNullAnnotation(FieldAndGetter fieldAndGetter) throws IllegalAccessException, InvocationTargetException {
        if (fieldAndGetter.field.isAnnotationPresent(NotNullInputField.class)){
            var value = fieldAndGetter.getter.invoke(this);
            this.checkIfNotNull(value, fieldAndGetter.field);
        }
    }

    private void checkIfNotNull(Object value, Field field){
        if (Optional.ofNullable(value).isEmpty())
            throw new NullFieldException(this.getFullFieldName(field));
    }

    public Optional<String> getResourceOwnerIdentifier() {
        //TODO: chained objects...
        return this.getFieldAndGetterList()
                .stream()
                .filter(fieldAndGetter -> fieldAndGetter.field.isAnnotationPresent(ResourceOwnerIdentifier.class))
                .map(fieldAndGetter -> {
                    try {
                        return fieldAndGetter.getter.invoke(this).toString();
                    } catch (Exception e) {
                        throw new InternalMappedException(
                                "Problem trying to invoke getter of '" + fieldAndGetter.field.getName()+"'",
                                "More details: " + e
                        );
                    }
                })
                .findFirst();
    }

    public Optional<String> getResourceIdentifier(){
        //TODO: chained objects...
        return this.getFieldAndGetterList()
                .stream()
                .filter(fieldAndGetter -> fieldAndGetter.field.isAnnotationPresent(ResourceIdentifier.class))
                .map(fieldAndGetter -> {
                    try {
                        return fieldAndGetter.getter.invoke(this).toString();
                    } catch (Exception e) {
                        throw new InternalMappedException(
                                "Problem trying to invoke getter of '" + fieldAndGetter.field.getName()+"'",
                                "More details: " + e
                        );
                    }
                })
                .findFirst();
    }

    public static class GetterMethodNotFoundException extends InternalMappedException {
        public GetterMethodNotFoundException(String fullFieldName) {
            super("Getter method not found for one of the fields.", "More details: the field '" + fullFieldName + "' has no getter method defined for it. Please define one method for this purpose.");
        }
    }

    public String getFullFieldName(Field field){
        return this.getClass().getSimpleName() + ":" + field.getName();
    }

    @Builder
    static class FieldAndGetter{
        private Field field;
        private Method getter;
    }

}
