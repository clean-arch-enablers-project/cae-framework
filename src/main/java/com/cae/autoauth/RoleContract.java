package com.cae.autoauth;

import java.util.List;

public interface RoleContract {

    String getRoleIdentifier();
    String getOwnerIdentifier();
    List<RoleStatementContract> getStatements();

}
