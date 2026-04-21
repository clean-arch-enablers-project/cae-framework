package com.cae.framework.autofeatures.autoauth.models;

import java.util.List;

public interface CaePolicy {

    String getId();
    boolean isAutomanaged();
    List<CaeStatement> getStatements();

}
