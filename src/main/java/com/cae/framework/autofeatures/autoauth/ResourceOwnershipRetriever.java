package com.cae.framework.autofeatures.autoauth;

import java.util.List;

public interface ResourceOwnershipRetriever {

    List<String> findByResourceId(Object resourceId);

}
