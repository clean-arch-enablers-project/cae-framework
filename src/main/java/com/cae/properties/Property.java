package com.cae.properties;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@Getter
@Builder(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Property {

    public static Property of(String name){
        return Property.builder()
                .name(name)
                .singleValue(false)
                .build();
    }

    public static Property ofSingleValue(String name, String value){
        Property.validatePropertyNameCandidate(name);
        Property.validatePropertyValueCandidate(value);
        var newProperty = Property.builder()
                .name(name)
                .singleValue(true)
                .build();
        newProperty.pairs.put("*", value);
        return newProperty;
    }

    private final String name;
    private final Map<String, String> pairs = new HashMap<>();
    private boolean singleValue;

    public Property setPair(String profile, String value){
        if (this.singleValue)
            throw new InternalMappedException(
                "The '" + this.name + "' is a single-value property",
                "You can't add new pairs of profile-value because it already has a profile set as '*' which overrides any profile"
            );
        else if (profile.equals("*"))
            throw new InternalMappedException(
                "Couldn't add profile-value pair for the '" + this.name + "' property",
                "Setting the profile manually as '*' is not allowed. If you need to create a single-value property, use the Property::ofSingleValue API"
            );
        if (this.pairs.containsKey(profile))
            throw new InternalMappedException(
                "Attempted to set profile '" + profile + "' more than once for the '" + this.name + "' property",
                "You can only set a profile once per property"
            );
        Property.validatePropertyProfileNameCandidate(profile);
        Property.validatePropertyValueCandidate(value);
        this.validateDuplicateValues(value);
        this.pairs.put(profile, value);
        return this;
    }

    private void validateDuplicateValues(String value) {
        var duplicates = this.pairs.values()
                .stream()
                .filter(previousValue -> previousValue.equals(value))
                .collect(Collectors.toList());
        if (!duplicates.isEmpty()){
            var logger = AutologProvider.SINGLETON.getProvidedInstance();
            var loggerWasProvided = logger.isPresent();
            Consumer<String> loggingAction = loggerWasProvided? logger.get()::logWarning : System.out::println;
            loggingAction.accept("CAE PROPERTIES MANAGEMENT WARNING: there are duplicated values for the '" + this.getName() + "' property (" + value + ")");
        }
    }


    public String getValueBy(String activeProfile) {
        if (this.isSingleValue())
            return this.pairs.get("*");
        return Optional.ofNullable(this.pairs.get(activeProfile))
                .orElseThrow(() -> new InternalMappedException(
                    "Couldn't get property value",
                    "The property '" + this.name + "' had no value for the active profile '" + activeProfile + "'"
                ));
    }

    private static void validatePropertyNameCandidate(String name){
        throwIfBlank(name, "name");
    }

    private static void validatePropertyValueCandidate(String value){
        throwIfBlank(value, "value");
    }

    private static void validatePropertyProfileNameCandidate(String profileName){
        throwIfBlank(profileName, "profile name");
    }

    private static void throwIfBlank(String someString, String type){
        if (someString.isBlank())
            throw new InternalMappedException(
                    "Can't create a property object with the provided " + type,
                    "It was blank"
            );
    }

}

