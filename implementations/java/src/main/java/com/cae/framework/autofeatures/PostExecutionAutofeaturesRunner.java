package com.cae.framework.autofeatures;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autolog.Autolog;
import com.cae.framework.autofeatures.autometrics.Autometrics;
import com.cae.framework.autofeatures.autonotify.Autonotify;

public class PostExecutionAutofeaturesRunner {

    private PostExecutionAutofeaturesRunner(){}

    public static void runOn(ExecutionContext executionContext){
        Autolog.runOn(executionContext);
        Autometrics.runOn(executionContext);
        Autonotify.runOn(executionContext);
    }

}
