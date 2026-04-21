package com.cae.framework.autofeatures.autoauth.models;

public interface CaeRole {

    String getId();
    String getOwnerId();
    CaePolicy getPolicy();

}
