package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface StatementGroup {

    String getId();
    boolean isShared();
    List<Statement> getStatements();

}
