package com.cae.use_cases.autodocumentation;

import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Builder
@Getter
public class DomainDocumentation {

    private final String domain;
    private final List<UseCaseDocumentation> useCases;

}
