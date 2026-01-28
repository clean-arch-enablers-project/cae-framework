package com.cae.framework.autofeatures.autodoc;


import com.cae.framework.autofeatures.autodoc.components.DomainDocumentation;
import com.cae.framework.autofeatures.autodoc.components.Responsible;
import com.cae.framework.autofeatures.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

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
            this.fileWriter = new BufferedWriter(new FileWriter("cae-autodoc.json"));
        } catch (IOException e) {
            throw new ExternalizeUseCaseException(e);
        }
    }

    public static DomainDocumentation run(
            String domainName,
            String projectPackage,
            Responsible responsible,
            boolean isKotlin) throws IOException, ClassNotFoundException {
        var registerer = new Autodoc();
        var fullDocumentation = AutodocGeneration.generateFor(domainName, projectPackage, responsible, isKotlin);
        registerer.startExternalization(fullDocumentation);
        registerer.endExternalization();
        return fullDocumentation;
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
