package com.cae.loggers.native_io_extraction_mode;

import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NativeExtractionMode {

    public static String executeOn(Object object){
        return SimpleJsonBuilder.buildFor(object);
    }

}
