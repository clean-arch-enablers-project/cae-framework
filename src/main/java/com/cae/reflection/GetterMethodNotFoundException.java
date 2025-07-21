package com.cae.reflection;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class GetterMethodNotFoundException extends InternalMappedException {
    public GetterMethodNotFoundException(String fullFieldName) {
        super("Getter method not found for one of the fields.", "More details: the field '" + fullFieldName + "' has no getter method defined for it. Please define one method for this purpose.");
    }
}
