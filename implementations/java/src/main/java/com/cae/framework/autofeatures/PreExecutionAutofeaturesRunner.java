package com.cae.framework.autofeatures;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autoauth.RoleBasedAutoauth;
import com.cae.framework.autofeatures.autoauth.ScopeBasedAutoauth;
import com.cae.framework.use_cases.UseCase;
import com.cae.framework.use_cases.UseCaseWithInput;
import com.cae.framework.use_cases.io.UseCaseInput;

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
