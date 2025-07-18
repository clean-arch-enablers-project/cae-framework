package com.cae.autofeatures.autodoc.components;

import lombok.*;

import java.util.List;

@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DomainDocumentation {

    private String domain;
    private List<EntityDocumentation> entities;
    private List<UseCaseDocumentation> useCases;
    private List<ArtifactDocumentation> supportingArtifacts;
    private Responsible responsible;

}
