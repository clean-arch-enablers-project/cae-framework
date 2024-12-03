package com.cae.use_cases.autodoc;


import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseCodeRetriever {

    public static String retrieveCodeFor(
            String useCaseImplementationLocation,
            String useCaseImplementationName,
            boolean kotlin) {
        var currentPath = System.getProperty("user.dir").replace("assemblers", "core");
        var location = currentPath +
                File.separator + "src" + File.separator + "main" + File.separator + (kotlin? "kotlin" : "java") + File.separator +
                useCaseImplementationLocation.replace(".", File.separator) + File.separator +
                useCaseImplementationName + (kotlin? ".kt" : ".java");
        var path = Paths.get(location);
        if (Files.exists(path)){
            return UseCaseCodeRetriever.getCodeFrom(path);
        }
        throw new InternalMappedException(
                "The file '" + path + "' couldn't be found. Make sure you are triggering the documentation process from the Assemblers layer.",
                "Your project structure should have 3 layers in order to use the GPT documentation mode: {domainName}-core, {domainName}-adapters and {domainName}-assemblers,"
        );
    }

    private static String getCodeFrom(Path path) {
        try {
            return Files.readString(path)
                    .replace("\n", " ")
                    .replace("\r", " ")
                    .replace("\t", " ")
                    .replace("\"", "\\\"");
        } catch (IOException e) {
            throw new InternalMappedException(
                    "Something went wrong while trying to retrieve use case code for GPT documentation",
                    "Path: " + path + " | Exception: " + e
            );
        }
    }
}
