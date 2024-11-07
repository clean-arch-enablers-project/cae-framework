package com.cae.use_cases.autoauth;

import java.util.List;

public interface RoleStatementContract {

    String getRoleStatementIdentifier();
    Boolean allows();
    List<String> getActionIds();
}
