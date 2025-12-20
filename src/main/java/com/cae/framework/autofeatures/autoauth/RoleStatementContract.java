package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface RoleStatementContract {

    String getRoleStatementIdentifier();
    Boolean allows();
    List<String> getActionIds();
}
