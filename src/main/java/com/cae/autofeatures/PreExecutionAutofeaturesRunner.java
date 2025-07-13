package com.cae.autofeatures;

import com.cae.autofeatures.autoauth.RoleBasedAutoauth;
import com.cae.autofeatures.autoauth.ScopeBasedAutoauth;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseWithInput;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

public class PreExecutionAutofeaturesRunner {

    private PreExecutionAutofeaturesRunner(){}

    public static void run(UseCaseInput input, ExecutionContext context, UseCase useCase){
        ScopeBasedAutoauth.handle(context, useCase);
        RoleBasedAutoauth.handle(input, context, (UseCaseWithInput) useCase);
    }

    public static void run(ExecutionContext context, UseCase useCase){
        ScopeBasedAutoauth.handle(context, useCase);
    }

}
