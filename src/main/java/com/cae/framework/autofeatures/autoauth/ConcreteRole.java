package com.cae.framework.autofeatures.autoauth;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements Role {

    private String id;
    private String ownerId;
    private StatementGroup statementGroup;

}
