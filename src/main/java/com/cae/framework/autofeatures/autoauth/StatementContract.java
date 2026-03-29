package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface StatementContract {

    String getId();
    boolean allows();
    List<String> getActionIds();
}
