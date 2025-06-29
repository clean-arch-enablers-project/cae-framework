package com.cae.autodoc;


import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CodeRetriever {

    public static String retrieveCodeFor(
            String implementationLocation,
            String implementationName,
            boolean kotlin) {
        var currentPath = System.getProperty("user.dir");
        if (!Files.exists(Paths.get(currentPath + File.separator + "cae-settings.json"))) throw new InternalMappedException(
                "Couldn't retrieve source code for " + implementationName,
                "Make sure you run this process in the root of your project structure and it has the cae-settings.json file"
        );
        var location = currentPath +
                File.separator + "src" + File.separator + "main" + File.separator + (kotlin? "kotlin" : "java") + File.separator +
                implementationLocation.replace(".", File.separator) + File.separator +
                implementationName + (kotlin? ".kt" : ".java");
        var path = Paths.get(location);
        if (Files.exists(path)){
            return CodeRetriever.getCodeFrom(path);
        }
        throw new InternalMappedException(
                "The file '" + path + "' couldn't be found.",
                "Make sure the file exists in the expected path"
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
