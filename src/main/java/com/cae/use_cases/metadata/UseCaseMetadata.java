package com.cae.use_cases.metadata;

import com.cae.use_cases.UseCase;

/**
 * The metadata of use cases such as name, description and protection status.
 * <p></p>
 * The name of use cases will be set in snake case.
 * <p></p>
 * Their descriptions are for providing a brief but comprehensible
 * overview about their essence.
 * <p></p>
 * The protection status is meant to inform if the use case execution
 * access is open or protected. If protected, it is up to the external layer
 * dispatching the use case to implement the access validation
 * to decide weather the action will be allowed or not.
 * <p></p>
 * All of this metadata can be externalized to a txt file at
 * /target if the use case registerer is set to be used.
 * It is up to you to decide what to do with the txt file. For
 * instance, it could be used to expose an overview about the
 * available use cases of the application for governance means.
 */
public class UseCaseMetadata {

    private final String name;
    private final Boolean isProtected;

    /**
     * Instantiates the use case metadata with the protected status set to true,
     * which means it will be considered that the use case is not set to open access.
     * @param useCaseType the use case class
     * @return the use case metadata instance
     * @param <U> the use case type
     */
    public static  <U extends UseCase> UseCaseMetadata ofProtectedUseCase(Class<U> useCaseType){
        return new UseCaseMetadata(useCaseType, true);
    }

    /**
     * Instantiates the use case metadata with the protected status set to false,
     * which means it will be considered that the use case is set to open access.
     * @param useCaseType the use case class
     * @return the use case metadata instance
     * @param <U> the use case type
     */
    public static <U extends UseCase> UseCaseMetadata ofOpenAccessUseCase(Class<U> useCaseType){
        return new UseCaseMetadata(useCaseType, false);
    }

    private <U extends UseCase> UseCaseMetadata(Class<U> useCaseType, Boolean isProtected) {
        this.name = getNameOutta(useCaseType);
        this.isProtected = isProtected;
    }

    private <U extends UseCase> String getNameOutta(Class<U> useCaseType) {
        var simpleName = useCaseType.getSimpleName();
        var snakeCaseName = new StringBuilder();
        var index = 0;
        for (var character : simpleName.toCharArray()){
            if (Character.isUpperCase(character) && index > 0)
                snakeCaseName.append("_");
            snakeCaseName.append(String.valueOf(character).toLowerCase());
            index ++;
        }
        return snakeCaseName.toString().replace("_use_case", "");
    }

    public String getName() {
        return name;
    }

    public Boolean isProtected() {
        return isProtected;
    }

}
