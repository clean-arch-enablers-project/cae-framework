package com.cae.autofeatures.autolog.native_io_extraction_mode.json;

import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.finalizers.CollectionJsonStructureFinalizer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.finalizers.CommonJsonStructureFinalizer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.finalizers.JsonStructureFinalizer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.finalizers.VerySimpleValueJsonStructureFinalizer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.initializers.CollectionJsonStructureInitializer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.initializers.CommonJsonStructureInitializer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.initializers.JsonStructureInitializer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.initializers.VerySimpleValueJsonStructureInitializer;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.reflections.FieldRetriever;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.reflections.GetterExtractor;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.reflections.GetterInvoker;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.reflections.ValueTypeScanner;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.sensitive.Sensitive;
import com.cae.autofeatures.autolog.native_io_extraction_mode.json.sensitive.SensitiveValueHandler;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;

@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class SimpleJsonBuilder {

    public static final int STARTING_POINT_FOR_COUNTER = 1;

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
        if (value == null)
            return "null";
        if (ValueTypeScanner.isSimpleValue(value))
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
        var counter = new GetterCounter();
        for (var getter : allGetters){
            var keyName = this.generateKeyBasedOn(getter);
            FieldRetriever.getField(keyName, this.levelZero).ifPresent(field -> {
                if (counter.getCount() > STARTING_POINT_FOR_COUNTER) this.appendSeparator();
                this.appendKeyToMapping(keyName);
                this.appendValueBasedOn(getter, field);
                counter.increment();
            });
        }
    }

    private void handleVerySimpleValue() {
        this.stringBuilder.append(this.levelZero);
    }

    private String getItemsFromCollection() {
        var items = new StringBuilder();
        var counter = 1;
        for (var item : (Collection<?>) this.levelZero){
            if (ValueTypeScanner.isSimpleValue(item))
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

    private void appendSeparator(){
        this.stringBuilder.append(", ");
    }

    private String generateKeyBasedOn(Method getter){
        var fieldName = getter.getName()
                .replace("get", "");
        return fieldName.substring(0, 1).toLowerCase() + fieldName.substring(1);
    }

    public void appendKeyToMapping(String key){
        this.stringBuilder.append("\"")
                .append(key)
                .append("\"")
                .append(": ");
    }

    private void appendValueBasedOn(Method getter, Field field) {
        var isSensitive = field.isAnnotationPresent(Sensitive.class);
        var value = GetterInvoker.execute(getter, this.levelZero);
        if (value == null)
            this.stringBuilder.append("null");
        else if (ValueTypeScanner.isSimpleValue(value))
            this.stringBuilder.append("\"")
                    .append(isSensitive? SensitiveValueHandler.handle(value, field) : value)
                    .append("\"");
        else
            this.stringBuilder.append(isSensitive? "\"***************\"" : SimpleJsonBuilder.buildFor(value));
    }

    @Getter
    static class GetterCounter{
        private Integer count = STARTING_POINT_FOR_COUNTER;

        public void increment(){
            this.count = count + 1;
        }

    }

}
