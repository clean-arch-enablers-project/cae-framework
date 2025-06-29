package com.cae.autofeatures.autoauth;

import java.util.List;

public interface RoleStatementContract {

    String getRoleStatementIdentifier();
    Boolean allows();
    List<String> getActionIds();
}
