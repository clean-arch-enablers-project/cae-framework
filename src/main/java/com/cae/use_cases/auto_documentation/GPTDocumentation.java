package com.cae.use_cases.auto_documentation;

import com.cae.env_vars.EnvVarRetriever;
import com.cae.http_client.HttpRequestStarter;
import com.cae.http_client.HttpResponse;
import com.cae.http_client.implementations.HttpRequestStarterImplementation;
import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.*;

import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GPTDocumentation {

    public static final GPTDocumentation SINGLETON = new GPTDocumentation();

    public static final String CAE_GPT_API_HOST = "CAE_GPT_API_HOST";

    private  final HttpRequestStarter httpRequestStarter = new HttpRequestStarterImplementation();

    public Output getGPTDescriptionAbout(Input input){
        input.handleString();
        var host = EnvVarRetriever.getEnvVarByNameAsString(CAE_GPT_API_HOST);
        var body = HttpRequest.BodyPublishers.ofString(SimpleJsonBuilder.buildFor(input), StandardCharsets.UTF_8);
        var request = this.httpRequestStarter.startPostRequestFor(host.concat("/analyze"), body)
                .headerOf("Content-Type", "application/json")
                .headerOf("Accept", "*/*")
                .handlerForAnyUnsuccessfulResponse(GPTDocumentation::handle)
                .finishBuildingModel();
        return request.sendRequestReturning(Output.class);
    }

    private static void handle(HttpResponse response) {
        throw new InternalMappedException("Something went wrong while trying to integrate with the GPT API", response.getStatusCode().toString());
    }

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    @Getter
    @Setter
    public static class Input {

        private String code;

        public static Input of(String code){
            return new Input(code);
        }

        public void handleString() {
            this.code = this.code.replace("\"", "\\\"")
                    .replace("\n", " ")
                    .replace("\r", " ")
                    .replace("\t", " ");
        }
    }

    @Getter
    @Setter
    public static class Output {

        private String analysis;

    }

}
