package com.cae.autodoc;

import lombok.*;

import java.util.List;

@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DomainDocumentation {

    private String domain;
    private List<UseCaseDocumentation> useCases;

}
