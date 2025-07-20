package com.cae.properties;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Properties {

    public static final Properties SINGLETON = new Properties();

    private List<Property> configs = new ArrayList<>();
    private String activeProfile;
    private boolean locked = false;

    public void setProperty(Property property){
        if (property.getPairs().isEmpty())
            throw new InternalMappedException(
                "Property '" + property.getName() + "' had no values",
                "Make sure it will have either a single value or values set by different profiles"
            );
        if (property.getPairs().size() == 1 && !property.isSingleValue())
            throw new InternalMappedException(
                "Property '" + property.getName() + "' only had 1 pair and the profile wasn't set as '*'",
                "When properties have a single value, their profile must be set as '*'. If you don't want to set it manually, use the CaeSetup::properties API"
            );
        this.configs.add(property);
    }

    public void setActiveProfile(String activeProfile){
        if (!this.locked){
            this.activeProfile = activeProfile;
            this.configs.forEach(this::setActualSystemProperty);
            this.locked = true;
        }
        else
            throw new InternalMappedException(
                    "The active profile was already set as " + this.activeProfile,
                    "You can only set the active profile once"
            );
    }

    private void setActualSystemProperty(Property property){
        System.setProperty(property.getName(), property.getValueBy(this.activeProfile));
    }

    public Optional<String> getActiveProfile(){
        return Optional.ofNullable(this.activeProfile);
    }

    public static Optional<String> get(String name){
        if (SINGLETON.getActiveProfile().isEmpty())
            throw new InternalMappedException(
                    "The currently active profile wasn't provided yet",
                    "Make sure to only start getting properties after having set the active profile"
            );
        return Optional.ofNullable(System.getProperty(name));
    }

    public static String getOrThrow(String name){
        return get(name).orElseThrow(() -> new MissingPropertyMappedException(name));
    }

    public void reset(){
        this.configs.forEach(this::removeActualSystemProperty);
        this.configs = new ArrayList<>();
        this.activeProfile = null;
        this.locked = false;
    }

    private void removeActualSystemProperty(Property property) {
        System.clearProperty(property.getName());
    }

}
