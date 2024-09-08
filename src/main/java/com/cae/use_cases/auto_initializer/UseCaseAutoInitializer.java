package com.cae.use_cases.auto_initializer;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseAutoInitializer {

    public static void initializeByAssemblerLayer(String useCasesPackageForAssemblersLayer) throws IOException, ClassNotFoundException {
        if (useCasesPackageForAssemblersLayer != null){
            getClasses(useCasesPackageForAssemblersLayer)
                    .stream()
                    .filter(clazz -> clazz.getSimpleName().endsWith("Assembler"))
                    .forEach(UseCaseAutoInitializer::initializeUseCase);
        }
    }

    private static List<Class<?>> getClasses(String packageName) throws IOException, ClassNotFoundException {
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

    private static void initializeUseCase(Class<?> clazz){
        var singletonInstanceField = Arrays.stream(clazz.getDeclaredFields())
                .filter(field -> field.getName().equals("SINGLETON_ASSEMBLER"))
                .findFirst()
                .orElseThrow(() -> new InternalMappedException("Assembler '" + clazz.getSimpleName() + "' had no SINGLETON_ASSEMBLER field", "Assemblers must have the SINGLETON_ASSEMBLER field"));
        try {
            singletonInstanceField.setAccessible(true);
            ((UseCaseAssembler<? extends UseCase>) singletonInstanceField.get(clazz)).getDefaultAssembledInstance();
        } catch (IllegalAccessException e) {
            throw new InternalMappedException("Something went wrong while trying to get the value of SINGLETON_ASSEMBLER field.", "Assembler class: " + clazz.getSimpleName());
        }
    }

}
