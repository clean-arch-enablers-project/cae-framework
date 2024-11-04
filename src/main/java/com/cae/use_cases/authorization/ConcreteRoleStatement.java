package com.cae.use_cases.authorization;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteRoleStatement implements RoleStatement{

    private String roleStatementIdentifier;
    private Boolean allows;
    private List<String> useCaseIds;

    public Boolean allows(){
        return this.allows;
    }

}
