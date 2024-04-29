package com.cae.use_cases.authorization;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

public abstract class UseCaseExecutionAuthorizer {

    protected abstract boolean isAllowed(UseCaseExecutionCorrelation useCaseExecutionCorrelation);




}
