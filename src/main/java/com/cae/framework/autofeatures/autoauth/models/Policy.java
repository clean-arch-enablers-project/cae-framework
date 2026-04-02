package com.cae.framework.autofeatures.autoauth.models;

import java.util.List;

public interface Policy {

    String getId();
    boolean isAutomanaged();
    List<Statement> getStatements();

}
