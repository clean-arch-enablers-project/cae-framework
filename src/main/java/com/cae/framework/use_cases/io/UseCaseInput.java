package com.cae.framework.use_cases.io;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autoauth.annotations.ResourceIdentifier;
import com.cae.framework.autofeatures.autoauth.annotations.ResourceOwnerIdentifiers;
import com.cae.framework.use_cases.io.annotations.NotBlankInputField;
import com.cae.framework.use_cases.io.annotations.NotEmptyInputField;
import com.cae.framework.use_cases.io.annotations.NotNullInputField;
import com.cae.framework.use_cases.io.annotations.ValidInnerPropertiesInputField;
import com.cae.framework.use_cases.io.exceptions.*;
import com.cae.mapped_exceptions.MappedException;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Builder;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
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

    public void autoverify(ExecutionContext executionContext){
        var autoverifyStep = executionContext.addStepInsightsOf(this.getClass().getSimpleName()+"::autoverify");
        try {
            for (var fieldAndGetter : this.getFieldAndGetterList()) {
                this.handleNotBlankAnnotation(fieldAndGetter);
                this.handleNotEmptyAnnotation(fieldAndGetter);
                this.handleNotNullAnnotation(fieldAndGetter);
                this.handleValidInnerPropertiesAnnotation(fieldAndGetter, executionContext);
            }
            this.validatePropertiesArbitrarily();
            autoverifyStep.complete();
        } catch (MappedException mappedException){
            autoverifyStep.complete(mappedException);
            throw mappedException;
        } catch (Exception e) {
            autoverifyStep.complete(e);
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

    private void handleValidInnerPropertiesAnnotation(FieldAndGetter fieldAndGetter, ExecutionContext executionContext) throws IllegalAccessException, InvocationTargetException {
        if (fieldAndGetter.field.isAnnotationPresent(ValidInnerPropertiesInputField.class)){
            Optional.ofNullable(fieldAndGetter.getter.invoke(this)).ifPresent(value -> {
                if (value instanceof UseCaseInput)
                    ((UseCaseInput) value).autoverify(executionContext);
                else if(value instanceof Collection){
                    for (var item : (Collection<?>) value)
                        this.handleCollectionItemValidation(item, fieldAndGetter.field, executionContext);
                }
                else
                    throw new ValidInnerPropertiesAnnotationOnWrongTypeException(this.getFullFieldName(fieldAndGetter.field));
            });
        }
    }

    private void handleCollectionItemValidation(Object item, Field field, ExecutionContext executionContext){
        if (item instanceof UseCaseInput)
            ((UseCaseInput) item).autoverify(executionContext);
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

    public List<String> getResourceOwnerIdentifiers() {
        //TODO: chained objects...
        var finalResult = new ArrayList<String>();
        List<?> partial = this.getFieldAndGetterList()
                .stream()
                .filter(fieldAndGetter -> fieldAndGetter.field.isAnnotationPresent(ResourceOwnerIdentifiers.class))
                .map(fieldAndGetter -> {
                    try {
                        if (fieldIsListOfStrings(fieldAndGetter))
                            return fieldAndGetter.getter.invoke(this);
                        throw new InternalMappedException(
                            "Unable to get resource owner identifiers",
                            "You can only annotate fields with @ResourceOwnerIdentifier when they are List<String>"
                        );
                    } catch (Exception e) {
                        throw new InternalMappedException(
                                "Problem trying to invoke getter of '" + fieldAndGetter.field.getName()+"'",
                                "More details: " + e
                        );
                    }
                })
                .map(List.class::cast)
                .findFirst()
                .orElse(new ArrayList<String>());
        partial.forEach(value -> finalResult.add(value.toString()));
        return finalResult;
    }

    private static boolean fieldIsListOfStrings(FieldAndGetter fieldAndGetter) {
        var field = fieldAndGetter.field;
        if (!List.class.isAssignableFrom(field.getType()))
            return false;
        var genericType = field.getGenericType();
        if (!(genericType instanceof ParameterizedType))
            return false;
        var pt = (ParameterizedType) genericType;
        var typeArgs = pt.getActualTypeArguments();
        if (typeArgs.length != 1)
            return false;
        return typeArgs[0] instanceof Class && typeArgs[0].equals(String.class);
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
