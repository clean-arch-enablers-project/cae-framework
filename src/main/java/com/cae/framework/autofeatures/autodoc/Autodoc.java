package com.cae.framework.autofeatures.autodoc;

import com.cae.framework.autofeatures.autodoc.annotations.AutodocSubject;
import com.cae.framework.autofeatures.autodoc.components.ArbitrarySubjectDocumentation;
import com.cae.framework.autofeatures.autodoc.components.BusinessEntityDocumentation;
import com.cae.framework.autofeatures.autodoc.components.Documentation;
import com.cae.framework.autofeatures.autodoc.components.UseCaseDocumentation;
import com.cae.framework.autofeatures.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.framework.entities.BusinessEntity;
import com.cae.framework.use_cases.ConsumerUseCase;
import com.cae.framework.use_cases.FunctionUseCase;
import com.cae.framework.use_cases.RunnableUseCase;
import com.cae.framework.use_cases.SupplierUseCase;
import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.io.*;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Autodoc implements Closeable {

    protected Autodoc(String rootPackage, Boolean isForJava, Boolean isForMaven) throws IOException {
        this.rootPackage = rootPackage;
        this.isForJava = isForJava;
        this.isForMaven = isForMaven;
        this.buildOutput = this.getBuildOutput();
        var generatedResourcesLocation = this.getGeneratedResourcesLocation();
        this.classpathBusinessEntitiesWriter = this.initializeClasspathBusinessEntitiesJsonlAt(generatedResourcesLocation);
        this.businessEntitiesWriter = this.initializeBusinessEntitiesJsonl();
        this.classpathUseCasesWriter = this.initializeClasspathUseCasesJsonlAt(generatedResourcesLocation);
        this.useCasesWriter = this.initializeUseCasesJsonl();
        this.classpathArbitrarySubjectsWriter = this.initializeClasspathArbitrarySubjectsJsonlAt(generatedResourcesLocation);
        this.arbitrarySubjectsWriter = this.initializeArbitrarySubjectsJsonl();
    }

    public static Autodoc of(
            String rootPackage,
            boolean isForJava,
            boolean isForMaven){
        try{
            return new Autodoc(rootPackage, isForJava, isForMaven);
        } catch (IOException e){
            throw new InternalMappedException(
                "Couldn't initialize autodoc",
                "Caught " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                e
            );
        }
    }

    private static final String CLASSPATH_JSONL_PREFIX = File.separator.concat("META-INF")
            .concat(File.separator)
            .concat("cae")
            .concat(File.separator)
            .concat("autodoc")
            .concat(File.separator);
    private static final String LOCAL_JSONL_PREFIX = File.separator.concat("cae-autodoc")
            .concat(File.separator);
    private static final String BUSINESS_ENTITIES_JSONL = "business_entities.jsonl";
    private static final String USE_CASES_JSONL = "use_cases.jsonl";
    private static final String ARBITRARY_SUBJECTS_JSONL = "arbitrary_subjects.jsonl";
    private static final String CLASSPATH_BUSINESS_ENTITIES_JSONL = CLASSPATH_JSONL_PREFIX + BUSINESS_ENTITIES_JSONL;
    private static final String CLASSPATH_USE_CASES_JSONL = CLASSPATH_JSONL_PREFIX + USE_CASES_JSONL;
    private static final String CLASSPATH_ARBITRARY_SUBJECTS_JSONL = CLASSPATH_JSONL_PREFIX + ARBITRARY_SUBJECTS_JSONL;


    private final StringBuilder buildOutput;
    private final String rootPackage;
    private final Boolean isForJava;
    private final Boolean isForMaven;
    private final BufferedWriter classpathBusinessEntitiesWriter;
    private final BufferedWriter businessEntitiesWriter;
    private final BufferedWriter classpathUseCasesWriter;
    private final BufferedWriter useCasesWriter;
    private final BufferedWriter classpathArbitrarySubjectsWriter;
    private final BufferedWriter arbitrarySubjectsWriter;

    public void run() throws IOException, ClassNotFoundException {
        var classesLocation = this.getClassesLocation();
        var classesLocationDir = new File(Paths.get(classesLocation).toUri());
        if (!classesLocationDir.exists())
            throw new InternalMappedException(
                "Couldn't run autodoc process",
                "The location '" + classesLocation + "' does not exist."
            );
        var subjectsWithinRoot = classesLocationDir.listFiles();
        if (subjectsWithinRoot == null)
            throw new InternalMappedException(
                "Couldn't run autodoc process",
                "The location '" + classesLocationDir.getAbsolutePath() + "' had no files within."
            );
        for (var subject : subjectsWithinRoot)
            this.handleYetToDiscoverSubject(subject, this.rootPackage);
        this.close();
    }

    private StringBuilder getBuildOutput() {
        var currentPath = System.getProperty("user.dir");
        return new StringBuilder()
                .append(currentPath)
                .append(File.separator)
                .append(this.isForMaven? "target" : "build");
    }

    private String getGeneratedResourcesLocation() {
        var suffixPath = this.isForMaven?
                ("classes") :
                ("resources" + File.separator + "main");
        return this.buildOutput.toString()
                .concat(File.separator)
                .concat(suffixPath);
    }

    private String getClassesLocation() {
        return this.buildOutput.toString()
                .concat(File.separator)
                .concat("classes")
                .concat(File.separator)
                .concat(this.rootPackage.replace('.', File.separatorChar));
    }

    private BufferedWriter initializeClasspathBusinessEntitiesJsonlAt(String generatedResourcesLocation) throws IOException {
        var businessEntitiesLocation = generatedResourcesLocation.concat(CLASSPATH_BUSINESS_ENTITIES_JSONL);
        Files.createDirectories(Paths.get(businessEntitiesLocation).getParent());
        return new BufferedWriter(new FileWriter(businessEntitiesLocation, true));
    }

    private BufferedWriter initializeBusinessEntitiesJsonl() throws IOException {
        var location = this.buildOutput.toString().concat(LOCAL_JSONL_PREFIX).concat(BUSINESS_ENTITIES_JSONL);
        Files.createDirectories(Paths.get(location).getParent());
        return new BufferedWriter(new FileWriter(location, true));
    }

    private BufferedWriter initializeClasspathUseCasesJsonlAt(String generatedResourcesLocation) throws IOException {
        var useCasesLocation = generatedResourcesLocation.concat(CLASSPATH_USE_CASES_JSONL);
        Files.createDirectories(Paths.get(useCasesLocation).getParent());
        return new BufferedWriter(new FileWriter(useCasesLocation, true));
    }

    private BufferedWriter initializeUseCasesJsonl() throws IOException {
        var location = this.buildOutput.toString().concat(LOCAL_JSONL_PREFIX).concat(USE_CASES_JSONL);
        Files.createDirectories(Paths.get(location).getParent());
        return new BufferedWriter(new FileWriter(location, true));
    }

    private BufferedWriter initializeClasspathArbitrarySubjectsJsonlAt(String generatedResourcesLocation) throws IOException {
        var arbitrarySubjectsLocation = generatedResourcesLocation.concat(CLASSPATH_ARBITRARY_SUBJECTS_JSONL);
        Files.createDirectories(Paths.get(arbitrarySubjectsLocation).getParent());
        return new BufferedWriter(new FileWriter(arbitrarySubjectsLocation, true));
    }

    private BufferedWriter initializeArbitrarySubjectsJsonl() throws IOException {
        var location = this.buildOutput.toString().concat(LOCAL_JSONL_PREFIX).concat(ARBITRARY_SUBJECTS_JSONL);
        Files.createDirectories(Paths.get(location).getParent());
        return new BufferedWriter(new FileWriter(location, true));
    }

    private void handleYetToDiscoverSubject(File subject, String dynamicPackage) throws ClassNotFoundException {
        if (subject.isDirectory()) {
            var newDynamicPackage = dynamicPackage
                    .concat(".")
                    .concat(subject.getName());
            var subjectsWithin = subject.listFiles();
            if (subjectsWithin != null){
                for (var innerSubject: subjectsWithin){
                    this.handleYetToDiscoverSubject(innerSubject, newDynamicPackage);
                }
            }
        }
        else if(subject.isFile())
            this.handleFileSubject(subject, dynamicPackage);
    }

    private void handleFileSubject(File subject, String dynamicPackage) throws ClassNotFoundException {
        if (subject.getName().endsWith(".class")) {
            var normalizedSubjectName = subject.getName().replace(".class", "");
            var fullyQualifiedClassName = dynamicPackage.concat(".").concat(normalizedSubjectName);
            var subjectClass = Class.forName(fullyQualifiedClassName);
            if (this.isBusinessEntity(subjectClass))
                this.writeBusinessEntityFoundIn(subjectClass);
            else if (this.isUseCase(subjectClass))
                this.writeUseCaseFoundIn(subjectClass);
            else if (this.isArbitrarySubject(subjectClass))
                this.writeArbitrarySubjectFoundIn(subjectClass);
        }
    }

    private boolean isBusinessEntity(Class<?> subjectClass) {
        return subjectClass.isAnnotationPresent(BusinessEntity.class);
    }

    private void writeBusinessEntityFoundIn(Class<?> subjectClass) {
        var businessEntityDocumentation = BusinessEntityDocumentation.of(subjectClass, this.isForJava);
        this.write(this.classpathBusinessEntitiesWriter, this.businessEntitiesWriter, businessEntityDocumentation);
    }

    private boolean isUseCase(Class<?> subjectClass) {
        return this.isConcrete(subjectClass) && this.extendsUseCase(subjectClass);
    }

    private boolean isConcrete(Class<?> subjectClass) {
        var modifiers = subjectClass.getModifiers();
        return (!subjectClass.isAnnotation() && !subjectClass.isInterface() && !subjectClass.isEnum() && !Modifier.isAbstract(modifiers));
    }

    private boolean extendsUseCase(Class<?> currentClass) {
        if (currentClass == null || currentClass == Object.class) return false;
        if (currentClass.getSuperclass() == null) return false;
        var superClass = currentClass.getSuperclass();
        if (superClass == FunctionUseCase.class || superClass == ConsumerUseCase.class || superClass == SupplierUseCase.class || superClass == RunnableUseCase.class) return true;
        return extendsUseCase(currentClass.getSuperclass());
    }

    private void writeUseCaseFoundIn(Class<?> subjectClass) {
        var useCaseDocumentation = UseCaseDocumentation.of(subjectClass, this.isForJava);
        this.write(this.classpathUseCasesWriter, this.useCasesWriter, useCaseDocumentation);
    }

    private boolean isArbitrarySubject(Class<?> subjectClass) {
        return subjectClass.isAnnotationPresent(AutodocSubject.class);
    }

    private void writeArbitrarySubjectFoundIn(Class<?> subjectClass) {
        var arbitrarySubjectDocumentation = ArbitrarySubjectDocumentation.of(subjectClass, this.isForJava);
        this.write(this.classpathArbitrarySubjectsWriter, this.arbitrarySubjectsWriter, arbitrarySubjectDocumentation);
    }

    private void write(BufferedWriter classpathWriter, BufferedWriter localWriter, Documentation documentation) {
        try {
            localWriter.write(SimpleJsonBuilder.buildFor(documentation));
            localWriter.newLine();
            documentation.cleanSourceCode();
            classpathWriter.write(SimpleJsonBuilder.buildFor(documentation));
            classpathWriter.newLine();
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to write content",
                    "Problematic writer: " + classpathWriter.getClass().getSimpleName() +
                    " | Problem: " + e.getClass().getSimpleName() + ": " + e.getMessage(),
                    e
            );
        }
    }

    @Override
    public void close() throws IOException {
        this.classpathBusinessEntitiesWriter.close();
        this.businessEntitiesWriter.close();
        this.classpathUseCasesWriter.close();
        this.useCasesWriter.close();
        this.classpathArbitrarySubjectsWriter.close();
        this.arbitrarySubjectsWriter.close();
    }
}
