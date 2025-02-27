package com.cae.autodoc;


import com.cae.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.use_cases.registries.UseCaseRegistry;
import com.cae.use_cases.assemblers.UseCaseAutoInitializer;

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
public class Autodoc {

    private final BufferedWriter fileWriter;

    private Autodoc(){
        try {
            this.fileWriter = new BufferedWriter(new FileWriter("cae-docfile.json"));
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }

    public static void run(String useCasesPackageForAssemblersLayer, String domainName, Boolean kotlin) throws IOException, ClassNotFoundException {
        UseCaseAutoInitializer.initializeByAssemblerLayer(useCasesPackageForAssemblersLayer);
        var registerer = new Autodoc();
        var useCasesDocumented = UseCaseRegistry.SINGLETON.getRegisteredUseCases()
                .stream()
                .map(useCase -> UseCaseDocumentationGenerator.generateFor(useCase, kotlin))
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
