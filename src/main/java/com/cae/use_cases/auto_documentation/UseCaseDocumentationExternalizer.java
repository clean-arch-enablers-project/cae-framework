package com.cae.use_cases.auto_documentation;


import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.use_cases.UseCaseRegistry;
import com.cae.use_cases.auto_initializer.UseCaseAutoInitializer;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.stream.Collectors;

/**
 * When set to run on some phase of your building process (e.g. Maven, etc.)
 * the public methods of this class will save each use case metadata
 * in a txt file at the /target folder. It is up to you what to do with
 * that file: real-time-up-to-date use case catalog of your applications
 * could be a nice way of using it.
 */
public class UseCaseDocumentationExternalizer {

    private final BufferedWriter fileWriter;

    private UseCaseDocumentationExternalizer(){
        try {
            this.fileWriter = new BufferedWriter(new FileWriter("target/cae-docfile.json"));
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }

    public static void externalize(String useCasesPackageForAssemblersLayer, String domainName) throws IOException, ClassNotFoundException {
        UseCaseAutoInitializer.initializeByAssemblerLayer(useCasesPackageForAssemblersLayer);
        var registerer = new UseCaseDocumentationExternalizer();
        var useCasesDocumented = UseCaseRegistry.SINGLETON.getRegisteredUseCases()
                .stream()
                .map(UseCaseDocumentationGenerator::generateFor)
                .collect(Collectors.toList());
        var fullDocumentation = DomainDocumentation.builder()
                .domain(domainName)
                .useCases(useCasesDocumented)
                .build();
        registerer.startExternalization(fullDocumentation);
        registerer.endExternalization();
    }

    private void startExternalization(DomainDocumentation fullDocumentation) {
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
