package com.cae.use_cases.autoauth;

import java.util.List;

public interface RoleStatement {

    String getRoleStatementIdentifier();
    Boolean allows();
    List<String> getUseCaseIds();
}
