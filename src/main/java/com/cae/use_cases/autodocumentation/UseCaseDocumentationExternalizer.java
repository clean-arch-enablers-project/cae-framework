package com.cae.use_cases.autodocumentation;


import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.use_cases.UseCaseRegistry;
import com.cae.use_cases.auto_initializer.UseCaseAutoInitializer;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

/**
 * When set to run on some phase of your building process (e.g. Maven, etc.)
 * the public methods of this class will save each use case metadata
 * in a txt file at the /target folder. It is up to you what to do with
 * that file: real-time-up-to-date use case catalog of your applications
 * could be a nice way of using it.
 */
public class UseCaseDocumentationExternalizer {

    public static final UseCaseDocumentationExternalizer SINGLETON = new UseCaseDocumentationExternalizer();

    private final BufferedWriter fileWriter;

    private UseCaseDocumentationExternalizer(){
        try {
            this.fileWriter = new BufferedWriter(new FileWriter("target/use_cases.json"));
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }

    public static void externalize(String useCasesPackageForAssemblersLayer) throws IOException, ClassNotFoundException {
        UseCaseAutoInitializer.initializeByAssemblerLayer(useCasesPackageForAssemblersLayer);
        var registerer = UseCaseDocumentationExternalizer.SINGLETON;
        var fullDocumentation = UseCaseRegistry.SINGLETON.getRegisteredUseCases()
                .stream()
                .map(UseCaseDocumentationGenerator::generateFor)
                .collect(Collectors.toList());
        registerer.startExternalization(fullDocumentation);
        registerer.endExternalization();
    }

    private void startExternalization(List<UseCaseDocumentation> fullDocumentation) {
        try {
            this.fileWriter.write(SimpleJsonBuilder.buildFor(fullDocumentation));
            this.fileWriter.newLine();
        } catch (Exception e) {
            throw new ExternalizeUseCaseException(e);
        }
    }

    private void endExternalization() {
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
