package com.cae.framework.autofeatures.autoauth;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements RoleContract {

    private String id;
    private String ownerId;
    private StatementGroupContract statementGroup;

}
