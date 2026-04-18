package com.cae.framework.autofeatures.autoauth.models;

import java.util.List;

public interface CaeStatement {

    String getId();
    boolean allows();
    List<String> getActionIds();
}
