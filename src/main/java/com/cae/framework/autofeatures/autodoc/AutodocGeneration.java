package com.cae.framework.autofeatures.autodoc;

import com.cae.framework.autofeatures.autodoc.annotations.AutodocSubject;
import com.cae.framework.autofeatures.autodoc.components.*;
import com.cae.framework.entities.BusinessEntity;
import com.cae.framework.use_cases.ConsumerUseCase;
import com.cae.framework.use_cases.FunctionUseCase;
import com.cae.framework.use_cases.RunnableUseCase;
import com.cae.framework.use_cases.SupplierUseCase;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class AutodocGeneration {

    public static DomainDocumentation generateFor(
            String domainName,
            String projectPackage,
            Responsible responsible,
            boolean isKotlin) throws IOException, ClassNotFoundException {
        var allClassesInPackage = getAllClassesIn(projectPackage);
        var allEntityClasses = filterEntities(allClassesInPackage);
        var useCaseImplementations = filterUseCaseImplementations(allClassesInPackage);
        var allArtifacts = filterArtifacts(allClassesInPackage);
        var allEntityDocs = allEntityClasses.stream()
                .map(entityClass -> EntityDocumentation.of(entityClass, isKotlin))
                .collect(Collectors.toList());
        var allUseCaseDocs = useCaseImplementations.stream()
                .map(useCaseClass -> UseCaseDocumentation.of(useCaseClass, isKotlin))
                .collect(Collectors.toList());
        var allArtifactDocs = allArtifacts.stream()
                .map(artifactClass -> ArbitrarySubjectDocumentation.of(artifactClass, isKotlin))
                .collect(Collectors.toList());
        return DomainDocumentation.builder()
                .domain(domainName)
                .responsible(responsible)
                .entities(allEntityDocs)
                .useCases(allUseCaseDocs)
                .otherSubjects(allArtifactDocs)
                .build();
    }

    private static List<Class<?>> getAllClassesIn(String packageName) throws IOException, ClassNotFoundException {
        var classes = new ArrayList<Class<?>>();
        var path = packageName.replace('.', '/');
        var resources = Thread.currentThread().getContextClassLoader().getResources(path);
        var directories = new ArrayList<File>();
        while (resources.hasMoreElements()) {
            var resource = resources.nextElement();
            directories.add(new File(resource.getFile()));
        }
        for (var directory : directories)
            classes.addAll(findClasses(directory, packageName));
        return classes;
    }

    private static List<Class<?>> findClasses(File directory, String packageName) throws ClassNotFoundException {
        var classes = new ArrayList<Class<?>>();
        if (!directory.exists())
            return classes;
        var files = directory.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    assert !file.getName().contains(".");
                    classes.addAll(findClasses(file, packageName + "." + file.getName()));
                } else if (file.getName().endsWith(".class")) {
                    String className = packageName + '.' + file.getName().substring(0, file.getName().length() - 6);
                    classes.add(Class.forName(className));
                }
            }
        }
        return classes;
    }

    private static List<Class<?>> filterEntities(List<Class<?>> allClasses){
        return allClasses.stream()
            .filter(AutodocGeneration::isBusinessEntity)
            .collect(Collectors.toList());
    }

    private static boolean isBusinessEntity(Class<?> currentClass) {
        return currentClass.isAnnotationPresent(BusinessEntity.class);
    }

    private static List<Class<?>> filterUseCaseImplementations(List<Class<?>> allClasses){
        return allClasses.stream()
            .filter(AutodocGeneration::isConcrete)
            .filter(AutodocGeneration::isUseCaseImplementation)
            .collect(Collectors.toList());
    }

    private static boolean isConcrete(Class<?> currentClass) {
        var modifiers = currentClass.getModifiers();
        return (!currentClass.isAnnotation() && !currentClass.isInterface() && !currentClass.isEnum() && !Modifier.isAbstract(modifiers));
    }

    private static boolean isUseCaseImplementation(Class<?> currentClass) {
        if (currentClass == null || currentClass == Object.class) return false;
        if (currentClass.getSuperclass() == null) return false;
        var superClass = currentClass.getSuperclass();
        if (superClass == FunctionUseCase.class || superClass == ConsumerUseCase.class || superClass == SupplierUseCase.class || superClass == RunnableUseCase.class) return true;
        return isUseCaseImplementation(currentClass.getSuperclass());
    }

    public static List<Class<?>> filterArtifacts(List<Class<?>> allClasses){
        return allClasses.stream()
                .filter(AutodocGeneration::isArtifact)
                .collect(Collectors.toList());
    }

    private static boolean isArtifact(Class<?> aClass) {
        return aClass.isAnnotationPresent(AutodocSubject.class);
    }

}
