package com.cae.use_cases.assemblers;

import com.cae.initializers.Lazy;
import com.cae.use_cases.UseCase;

public interface UseCaseAssembler<T extends UseCase> {
    Lazy<T> getDefaultAssembledInstance();

}
