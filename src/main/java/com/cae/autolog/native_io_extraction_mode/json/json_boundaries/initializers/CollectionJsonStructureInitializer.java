package com.cae.loggers.native_io_extraction_mode.json.json_boundaries.initializers;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CollectionJsonStructureInitializer implements JsonStructureInitializer {

    public static final JsonStructureInitializer SINGLETON = new CollectionJsonStructureInitializer();

    @Override
    public StringBuilder execute() {
        return new StringBuilder("[ ");
    }
}
