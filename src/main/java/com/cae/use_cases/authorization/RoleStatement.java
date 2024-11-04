package com.cae.use_cases.authorization;

import java.util.List;

public interface RoleStatement {

    String getRoleStatementIdentifier();
    Boolean allows();
    List<String> getUseCaseIds();
}
