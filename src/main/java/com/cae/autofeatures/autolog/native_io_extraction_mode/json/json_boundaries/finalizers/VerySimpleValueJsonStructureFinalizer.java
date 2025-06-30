package com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.finalizers;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class VerySimpleValueJsonStructureFinalizer implements JsonStructureFinalizer {

    public static final JsonStructureFinalizer SINGLETON = new VerySimpleValueJsonStructureFinalizer();

    @Override
    public void execute(StringBuilder stringBuilder) {
        stringBuilder.append("\"");
    }
}
