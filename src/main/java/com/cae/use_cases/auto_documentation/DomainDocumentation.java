package com.cae.use_cases.auto_documentation;

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
