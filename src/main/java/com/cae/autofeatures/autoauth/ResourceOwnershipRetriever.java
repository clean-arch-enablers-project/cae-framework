package com.cae.autofeatures.autoauth;

import java.util.Optional;

public interface ResourceOwnershipRetriever {

    Optional<String> findByResourceId(Object resourceId);

}
