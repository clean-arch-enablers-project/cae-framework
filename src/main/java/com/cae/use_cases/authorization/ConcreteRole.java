package com.cae.use_cases.authorization;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements Role{

    private String roleIdentifier;
    private String ownerIdentifier;
    private List<RoleStatement> statements;

}
