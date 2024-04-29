package com.cae.loggers.native_io_extraction_mode.json;

import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.finalizers.CollectionJsonStructureFinalizer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.finalizers.CommonJsonStructureFinalizer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.finalizers.JsonStructureFinalizer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.finalizers.VerySimpleValueJsonStructureFinalizer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.initializers.CollectionJsonStructureInitializer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.initializers.CommonJsonStructureInitializer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.initializers.JsonStructureInitializer;
import com.cae.loggers.native_io_extraction_mode.json.json_boundaries.initializers.VerySimpleValueJsonStructureInitializer;
import com.cae.loggers.native_io_extraction_mode.json.reflections.FieldRetriever;
import com.cae.loggers.native_io_extraction_mode.json.reflections.GetterExtractor;
import com.cae.loggers.native_io_extraction_mode.json.reflections.GetterInvoker;
import com.cae.loggers.native_io_extraction_mode.json.reflections.ValueTypeScanner;
import com.cae.loggers.native_io_extraction_mode.json.sensitive.Sensitive;
import com.cae.loggers.native_io_extraction_mode.json.sensitive.SensitiveValueHandler;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.lang.reflect.Method;
import java.util.Collection;

@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class SimpleJsonBuilder {

    private static SimpleJsonBuilder ofCollection(Object levelZero){
        return new SimpleJsonBuilder(
                true,
                false,
                CollectionJsonStructureInitializer.SINGLETON,
                CollectionJsonStructureFinalizer.SINGLETON,
                levelZero
        ).getInitialized();
    }

    private static SimpleJsonBuilder ofCommonObject(Object levelZero){
        return new SimpleJsonBuilder(
                false,
                false,
                CommonJsonStructureInitializer.SINGLETON,
                CommonJsonStructureFinalizer.SINGLETON,
                levelZero
        ).getInitialized();
    }

    private static SimpleJsonBuilder ofSimpleValue(Object simpleValue){
        return new SimpleJsonBuilder(
                false,
                true,
                VerySimpleValueJsonStructureInitializer.SINGLETON,
                VerySimpleValueJsonStructureFinalizer.SINGLETON,
                simpleValue
        ).getInitialized();
    }

    public static String buildFor(Object value){
        if (Boolean.TRUE.equals(ValueTypeScanner.isSimpleValue(value)))
            return String.valueOf(value);
        var builder = value instanceof Collection? SimpleJsonBuilder.ofCollection(value) : SimpleJsonBuilder.ofCommonObject(value);
        return builder.build();
    }

    private final boolean ofCollection;
    private final boolean ofVerySimpleValue;
    private final JsonStructureInitializer initializer;
    private final JsonStructureFinalizer finalizer;
    private StringBuilder stringBuilder;
    private final Object levelZero;

    private SimpleJsonBuilder getInitialized(){
        this.stringBuilder = this.initializer.execute();
        return this;
    }

    public String build(){
        if (this.isOfCollection())
            this.handleCollection();
        else if (this.isOfVerySimpleValue())
            this.handleVerySimpleValue();
        else
            this.handleCommonObject();
        this.finalizer.execute(this.stringBuilder);
        return this.stringBuilder.toString();
    }

    private void handleCollection() {
        this.append(this.getItemsFromCollection());
    }

    private void handleCommonObject() {
        var allGetters = GetterExtractor.executeOn(levelZero);
        var counter = 1;
        for (var getter : allGetters){
            var keyName = this.generateKeyBasedOn(getter);
            this.generateValueBasedOn(getter, keyName);
            if (counter < allGetters.size()) this.generateSeparator();
            counter ++;
        }
    }

    private void handleVerySimpleValue() {
        this.stringBuilder.append(this.levelZero);
    }

    private String getItemsFromCollection() {
        var items = new StringBuilder();
        var counter = 1;
        for (var item : (Collection<?>) this.levelZero){
            if (Boolean.TRUE.equals(ValueTypeScanner.isSimpleValue(item)))
                items.append(SimpleJsonBuilder.ofSimpleValue(item).build());
            else
                items.append(SimpleJsonBuilder.buildFor(item));
            if (counter != ((Collection<?>) this.levelZero).size())
                items.append(", ");
            counter++;
        }
        return items.toString();
    }

    private void append(Object value){
        this.stringBuilder.append(value);
    }

    private void generateSeparator(){
        this.stringBuilder.append(", ");
    }

    private String generateKeyBasedOn(Method getter){
        var fieldName = getter.getName()
                .replace("get", "");
        fieldName = fieldName.substring(0, 1).toLowerCase() + fieldName.substring(1);
        this.stringBuilder.append("\"")
                .append(fieldName)
                .append("\"")
                .append(": ");
        return fieldName;
    }

    private void generateValueBasedOn(Method getter, String keyName) {
        var field = FieldRetriever.getField(keyName, this.levelZero);
        var isSensitive = field.isAnnotationPresent(Sensitive.class);
        var value = GetterInvoker.execute(getter, this.levelZero);
        if (Boolean.TRUE.equals(ValueTypeScanner.isSimpleValue(value)))
            this.stringBuilder.append("\"")
                    .append(Boolean.TRUE.equals(isSensitive)? SensitiveValueHandler.handle(value, field) : value)
                    .append("\"");
        else
            this.stringBuilder.append(Boolean.TRUE.equals(isSensitive)? "\"***************\"" : SimpleJsonBuilder.buildFor(value));
    }

}
