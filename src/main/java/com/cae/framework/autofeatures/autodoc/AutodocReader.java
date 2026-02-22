package com.cae.framework.autofeatures.autodoc;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

public class AutodocReader implements Closeable {

    public static AutodocReader ofNew(){
        return new AutodocReader();
    }

    public static AutodocReader ofNew(String buildOutput) throws FileNotFoundException {
        return new AutodocReader(buildOutput);
    }

    private static final String CLASSPATH_PREFIX = "META-INF/cae/autodoc/";
    private static final String LOCAL_PREFIX = File.separator.concat("cae-autodoc").concat(File.separator);
    private static final String BUSINESS_ENTITIES = "business_entities.jsonl";
    private static final String CLASSPATH_BUSINESS_ENTITIES = CLASSPATH_PREFIX + BUSINESS_ENTITIES;
    private static final String USE_CASES = "use_cases.jsonl";
    private static final String CLASSPATH_USE_CASES = CLASSPATH_PREFIX + USE_CASES;
    private static final String ARBITRARY_SUBJECTS = "arbitrary_subjects.jsonl";
    private static final String CLASSPATH_ARBITRARY_SUBJECTS = CLASSPATH_PREFIX + ARBITRARY_SUBJECTS;

    private final BufferedReader businessEntitiesReaderFromClasspath;
    private final BufferedReader useCasesReaderFromClasspath;
    private final BufferedReader arbitrarySubjectsReaderFromClasspath;
    private final String buildOutput;
    private final BufferedReader businessEntitiesReaderFromBuildOutput;
    private final BufferedReader useCasesReaderFromBuildOutput;
    private final BufferedReader arbitrarySubjectsReaderFromBuildOutput;

    public AutodocReader() {
        this.businessEntitiesReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_BUSINESS_ENTITIES);
        this.useCasesReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_USE_CASES);
        this.arbitrarySubjectsReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_ARBITRARY_SUBJECTS);
        this.buildOutput = null;
        this.businessEntitiesReaderFromBuildOutput = null;
        this.useCasesReaderFromBuildOutput = null;
        this.arbitrarySubjectsReaderFromBuildOutput = null;
    }

    public AutodocReader(String buildOutputPath) throws FileNotFoundException {
        this.businessEntitiesReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_BUSINESS_ENTITIES);
        this.useCasesReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_USE_CASES);
        this.arbitrarySubjectsReaderFromClasspath = this.initializeReaderFromClasspath(CLASSPATH_ARBITRARY_SUBJECTS);
        this.buildOutput = buildOutputPath;
        this.businessEntitiesReaderFromBuildOutput = this.initializeReaderFromBuildOutput(BUSINESS_ENTITIES);
        this.useCasesReaderFromBuildOutput = this.initializeReaderFromBuildOutput(USE_CASES);
        this.arbitrarySubjectsReaderFromBuildOutput = this.initializeReaderFromBuildOutput(ARBITRARY_SUBJECTS);
    }

    private BufferedReader initializeReaderFromClasspath(String name){
        var classLoader = Thread.currentThread().getContextClassLoader();
        var inputStream = classLoader.getResourceAsStream(name);
        if (inputStream == null)
            throw new InternalMappedException(
                "Couldn't initialize reader for '" + name + "', from autodoc metadata",
                "Its InputStream was null"
            );
        return new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8));
    }

    private BufferedReader initializeReaderFromBuildOutput(String name) throws FileNotFoundException {
        var fullPath = Optional.ofNullable(this.buildOutput)
                .orElseThrow(() -> new InternalMappedException(
                    "Can't read from the build output",
                    "You have initialized the AutodocReader with no build output provided. " +
                    "Please consider providing one at the constructor if you desire to read the metadata from the build output"
                ))
                .concat(LOCAL_PREFIX)
                .concat(name);
        var inputStream = new FileInputStream(fullPath);
        return new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8));
    }

    public Optional<String> getNextBusinessEntityFromClasspath(){
        try {
            return Optional.ofNullable(this.businessEntitiesReaderFromClasspath.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                "Something went wrong while trying to read next business entity",
                "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                e
            );
        }
    }

    public Optional<String> getNextBusinessEntityFromBuildOutput(){
        this.throwIfBuildOutputIsNull();
        try {
            return Optional.ofNullable(this.businessEntitiesReaderFromBuildOutput.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to read next business entity",
                    "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    public Optional<String> getNextUseCaseFromClasspath(){
        try {
            return Optional.ofNullable(this.useCasesReaderFromClasspath.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to read next use case",
                    "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    public Optional<String> getNextUseCaseFromBuildOutput(){
        this.throwIfBuildOutputIsNull();
        try {
            return Optional.ofNullable(this.useCasesReaderFromBuildOutput.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to read next use case",
                    "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    public Optional<String> getNextArbitrarySubjectFromClasspath(){
        try {
            return Optional.ofNullable(this.arbitrarySubjectsReaderFromClasspath.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to read next arbitrary subject",
                    "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    public Optional<String> getNextArbitrarySubjectFromBuildOutput(){
        this.throwIfBuildOutputIsNull();
        try {
            return Optional.ofNullable(this.arbitrarySubjectsReaderFromBuildOutput.readLine());
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to read next arbitrary subject",
                    "More details: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    private void throwIfBuildOutputIsNull() {
        if (this.buildOutput == null)
            throw new InternalMappedException(
                "Build output is null",
                "If you mean to read from the build output, you must provide its path via the AutodocReader constructor"
            );
    }

    @Override
    public void close() throws IOException {
        this.businessEntitiesReaderFromClasspath.close();
        this.useCasesReaderFromClasspath.close();
        this.arbitrarySubjectsReaderFromClasspath.close();
        if (this.businessEntitiesReaderFromBuildOutput != null)
            this.businessEntitiesReaderFromBuildOutput.close();
        if (this.useCasesReaderFromBuildOutput != null)
            this.useCasesReaderFromBuildOutput.close();
        if (this.arbitrarySubjectsReaderFromBuildOutput != null)
            this.arbitrarySubjectsReaderFromBuildOutput.close();
    }
}
