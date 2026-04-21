package com.cae.framework.use_cases.assemblers;

import com.cae.framework.initializers.Lazy;
import com.cae.framework.use_cases.UseCase;

public interface UseCaseAssembler<T extends UseCase> {
    Lazy<T> getDefaultAssembledInstance();

}
