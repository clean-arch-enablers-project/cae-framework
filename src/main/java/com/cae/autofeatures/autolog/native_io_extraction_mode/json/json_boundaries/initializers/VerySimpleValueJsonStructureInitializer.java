package com.cae.autofeatures.autolog.native_io_extraction_mode.json.json_boundaries.initializers;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class VerySimpleValueJsonStructureInitializer implements JsonStructureInitializer {

    public static final JsonStructureInitializer SINGLETON = new VerySimpleValueJsonStructureInitializer();

    @Override
    public StringBuilder execute() {
        return new StringBuilder("\"");
    }
}
