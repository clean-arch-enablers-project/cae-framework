package com.cae.framework.autofeatures.autoauth.models;

import java.util.List;

public interface Statement {

    String getId();
    boolean allows();
    List<String> getActionIds();
}
