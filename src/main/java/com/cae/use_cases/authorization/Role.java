package com.cae.use_cases.authorization;

import java.util.List;

public interface Role {

    String getRoleIdentifier();
    String getOwnerIdentifier();
    List<RoleStatement> getStatements();

}
