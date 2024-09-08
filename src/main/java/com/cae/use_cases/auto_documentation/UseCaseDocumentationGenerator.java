package com.cae.use_cases.auto_documentation;

import com.cae.use_cases.UseCase;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseDocumentationGenerator {

    public static UseCaseDocumentation generateFor(UseCase useCase, boolean kotlin){
        var documentation = UseCaseDocumentation.of(useCase, kotlin);
//        if (System.getenv(CAE_GPT_API_HOST) != null){
//            var gptAnalysis = GPTDocumentation.SINGLETON.getGPTDescriptionAbout(GPTDocumentation.Input.of(documentation.getUseCaseSourceCode()));
//            documentation.setDescription(gptAnalysis.getAnalysis().replace("\"", "\\\"")
//                    .replace("\n", " -||- ")
//                    .replace("\r", " ---|||--- ")
//                    .replace("\t", " ----||||---- "));
//        }
        documentation.setDescription("To get the workflow description in natural language, enable the GPT extraction mode.");
        return documentation;
    }

}
