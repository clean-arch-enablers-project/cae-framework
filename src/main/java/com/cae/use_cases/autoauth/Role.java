package com.cae.use_cases.autoauth;

import java.util.List;

public interface Role {

    String getRoleIdentifier();
    String getOwnerIdentifier();
    List<RoleStatement> getStatements();

}
