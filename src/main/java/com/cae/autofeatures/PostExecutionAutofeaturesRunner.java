package com.cae.autofeatures;

import com.cae.autofeatures.autolog.Autolog;
import com.cae.autofeatures.autometrics.Autometrics;
import com.cae.autofeatures.autonotify.Autonotify;
import com.cae.use_cases.contexts.ExecutionContext;

public class PostExecutionAutofeaturesRunner {

    private PostExecutionAutofeaturesRunner(){}

    public static void runOn(ExecutionContext executionContext){
        Autolog.runOn(executionContext);
        Autometrics.runOn(executionContext);
        Autonotify.runOn(executionContext);
    }

}
