package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface StatementGroupContract {

    String getId();
    boolean isShared();
    List<StatementContract> getStatements();

}
