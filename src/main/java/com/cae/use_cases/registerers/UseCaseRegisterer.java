package com.cae.use_cases.registerers;


import com.cae.use_cases.UseCase;
import com.cae.use_cases.metadata.UseCaseMetadata;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

/**
 * When set to run on some phase of your building process (e.g. Maven, etc.)
 * the public methods of this class will save each use case metadata
 * in a txt file at the /target folder. It is up to you what to do with
 * that file: real-time-up-to-date use case catalog of your applications
 * could be a nice way of using it.
 */
public class UseCaseRegisterer {
    private final BufferedWriter fileWriter;

    /**
     * Will save each use case metadata in the txt file at /target folder
     * @param useCases use case instances
     */
    public static void runOnUseCases(List<UseCase> useCases) {
        var registerer = UseCaseRegisterer.defaultInstance();
        useCases.stream()
                .map(UseCase::getUseCaseMetadata)
                .forEach(registerer::externalizeUseCase);
        registerer.endRegistering();
    }

    /**
     * Will save each use case metadata in the txt file at /target folder
     * @param useCasesMetadata use case metadata instances
     */
    public static void runOnUseCasesMetadata(List<UseCaseMetadata> useCasesMetadata) {
        var registerer = UseCaseRegisterer.defaultInstance();
        useCasesMetadata.forEach(registerer::externalizeUseCase);
        registerer.endRegistering();
    }
    private static UseCaseRegisterer defaultInstance() {
        return new UseCaseRegisterer();
    }
    private UseCaseRegisterer(){
        try {
            this.fileWriter = new BufferedWriter(new FileWriter("target/use_cases.txt"));
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }
    private void externalizeUseCase(UseCaseMetadata useCaseMetadata) {
        try {
            this.fileWriter.write("| Name: " + useCaseMetadata.getName() + " | Description: " + useCaseMetadata.getDescription() + " | Protected: " + useCaseMetadata.isProtected());
            this.fileWriter.newLine();
        } catch (Exception e) {
            throw new ExternalizeUseCaseException(e);
        }
    }
    private void endRegistering() {
        try {
            this.fileWriter.close();
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }
    protected static class ExternalizeUseCaseException extends RuntimeException {
        public ExternalizeUseCaseException(Exception e) {
            super("Something went wrong while trying to externalize use cases info. More details: " + e.toString());
        }
    }

}
