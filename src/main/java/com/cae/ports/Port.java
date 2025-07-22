package com.cae.ports;

import com.cae.autofeatures.autocache.Cacheable;
import com.cae.autofeatures.autocache.annotations.Autocache;
import com.cae.autofeatures.autocache.metadata.AutocacheMetadata;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;

/**
 * Ports are meant to be the bridge between your core components
 * and the outside world. Ports come in a couple flavours: functions,
 * consumers, suppliers and runnables. Those subtypes are available
 * as specifics.
 */
@Getter
public abstract class Port {

    /**
     * Name of the implementation. If you need to find something
     * by ID within some use case implementation, accessing a database
     * or an API for that purpose, you would call a port from there instead
     * of calling the finding-by-id method implementation. The name of
     * the class would be set here, in this attribute, based on the class name
     * itself.
     */
    protected final String name;
    protected final AutocacheMetadata autocacheMetadata;

    protected Port() {
        var clazz = this.getClass();
        this.name = clazz.getSimpleName();
        this.autocacheMetadata = isAutocacheAnnotated(clazz)? AutocacheMetadata.of(Port.getAutocacheAnnotationFrom(clazz)) : null;
    }

    private static Autocache getAutocacheAnnotationFrom(Class<?> clazz) {
        var annotation = clazz.getAnnotation(Autocache.class);
        if (annotation == null && clazz != Port.class)
            return Port.getAutocacheAnnotationFrom(clazz.getSuperclass());
        return annotation;
    }

    private static boolean isAutocacheAnnotated(Class<?> portType) {
        var isAnnotated = portType.isAnnotationPresent(Autocache.class);
        if (isAnnotated && Cacheable.class.isAssignableFrom(portType))
            return true;
        else if (isAnnotated && !(Cacheable.class.isAssignableFrom(portType)))
            throw new InternalMappedException(
                    "Autocache annotation used on wrong type of Port",
                    "Autocache is only allowed on port instances that extend the Cacheable interface"
            );
        if (portType == Port.class)
            return false;
        return Port.isAutocacheAnnotated(portType.getSuperclass());
    }

    protected boolean usesAutocache(){
        return this.autocacheMetadata != null;
    }

}
