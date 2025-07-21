package com.cae.autofeatures.autocache;

import com.cae.reflection.FieldAndGetterExtractor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AutocacheKeyExtractor {

    public static String runOn(Object object){
        var fieldsAndGetter = FieldAndGetterExtractor.runOn(object);
        return null;
    }

}
