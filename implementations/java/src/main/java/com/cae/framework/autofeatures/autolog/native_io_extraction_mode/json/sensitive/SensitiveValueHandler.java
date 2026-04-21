package com.cae.framework.autofeatures.autolog.native_io_extraction_mode.json.sensitive;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.reflect.Field;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class SensitiveValueHandler {

    public static String handle(Object value, Field field){
        var defaultMaskedAmount = field.getAnnotation(Sensitive.class).defaultMaskedAmount();
        if (defaultMaskedAmount > 0) return "*".repeat(defaultMaskedAmount);
        else{
            var unmaskedAmount = field.getAnnotation(Sensitive.class).unmaskedAmount();
            var fromLeft = field.getAnnotation(Sensitive.class).unmaskFromLeft();
            var original = String.valueOf(value);
            int maskedAmount = original.length() - unmaskedAmount;
            var replacement = "*".repeat(maskedAmount);
            if (fromLeft)
                return original.substring(0, original.length() - maskedAmount) + replacement;
            else
                return replacement + original.substring(maskedAmount);
        }
    }

}
