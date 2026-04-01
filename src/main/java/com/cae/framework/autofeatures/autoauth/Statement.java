package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface Statement {

    String getId();
    boolean allows();
    List<String> getActionIds();
}
