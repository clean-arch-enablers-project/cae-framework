package com.cae.use_cases.autoauth;

import lombok.*;

import java.util.List;
import java.util.stream.Collectors;

@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements RoleContract {

    @Getter
    private String roleIdentifier;
    @Getter
    private String ownerIdentifier;
    private List<ConcreteRoleStatement> statements;

    public List<RoleStatementContract> getStatements(){
        return this.statements.stream()
                .map(a -> (RoleStatementContract) a)
                .collect(Collectors.toList());
    }

}
