package com.cae.framework.autofeatures.autodoc.components;

import lombok.*;

import java.util.List;

@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DomainDocumentation {

    private String domain;
    private List<BusinessEntityDocumentation> entities;
    private List<UseCaseDocumentation> useCases;
    private List<ArbitrarySubjectDocumentation> otherSubjects;
    private Responsible responsible;

}
