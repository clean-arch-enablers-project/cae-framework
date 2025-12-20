package com.cae.framework.autofeatures.autoauth;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteRoleStatement implements RoleStatementContract {

    private String roleStatementIdentifier;
    private Boolean allows;
    private List<String> actionIds;

    public Boolean allows(){
        return this.allows;
    }

}
