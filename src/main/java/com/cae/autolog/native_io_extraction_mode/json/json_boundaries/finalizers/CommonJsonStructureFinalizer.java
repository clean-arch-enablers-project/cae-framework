package com.cae.loggers.native_io_extraction_mode.json.json_boundaries.finalizers;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommonJsonStructureFinalizer implements JsonStructureFinalizer {

    public static final JsonStructureFinalizer SINGLETON = new CommonJsonStructureFinalizer();

    @Override
    public void execute(StringBuilder stringBuilder) {
        stringBuilder.append(" }");
    }
}
