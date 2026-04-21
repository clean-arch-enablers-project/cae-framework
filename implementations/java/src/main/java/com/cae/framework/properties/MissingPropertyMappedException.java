package com.cae.framework.properties;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class MissingPropertyMappedException extends InternalMappedException {

    public MissingPropertyMappedException(String propertyName) {
        super(
            "Couldn't find property '" + propertyName + "'",
            "Make sure it is being set at the CaeSetup::properties API or natively at the System::setProperty API"
        );
    }
}
