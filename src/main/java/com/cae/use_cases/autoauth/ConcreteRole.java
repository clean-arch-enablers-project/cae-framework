package com.cae.use_cases.autoauth;

import lombok.*;

import java.util.List;
import java.util.stream.Collectors;

@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements RoleContract {

    @Getter
    @Setter
    private String roleIdentifier;

    @Getter
    @Setter
    private String ownerIdentifier;
    private List<ConcreteRoleStatement> statements;

    public List<RoleStatementContract> getStatements(){
        return this.statements.stream()
                .map(a -> (RoleStatementContract) a)
                .collect(Collectors.toList());
    }

    public void setStatements(List<ConcreteRoleStatement> statements){
        this.statements = statements;
    }

}
