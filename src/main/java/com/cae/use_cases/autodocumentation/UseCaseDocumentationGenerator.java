package com.cae.use_cases.autodocumentation;

import com.cae.use_cases.UseCase;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import static com.cae.use_cases.autodocumentation.GPTDocumentation.CAE_GPT_API_HOST;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseDocumentationGenerator {

    public static UseCaseDocumentation generateFor(UseCase useCase){
        var documentation = UseCaseDocumentation.of(useCase);
        if (System.getenv(CAE_GPT_API_HOST) != null){
            var code = UseCaseCodeRetriever.retrieveCodeFor(documentation);
            var gptAnalysis = GPTDocumentation.SINGLETON.getGPTDescriptionAbout(GPTDocumentation.Input.of(code));
            documentation.setDescription(gptAnalysis.getAnalysis().replace("\"", "\\\"")
                    .replace("\n", " -||- ")
                    .replace("\r", " ---|||--- ")
                    .replace("\t", " ----||||---- "));
        }
        else
            documentation.setDescription("To get the workflow description in natural language, enable the GPT extraction mode.");
        return documentation;
    }

}
