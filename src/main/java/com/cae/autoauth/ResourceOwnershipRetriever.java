package com.cae.use_cases.autoauth;

import java.util.Optional;

public interface ResourceOwnershipRetriever {

    Optional<String> findByResourceId(Object resourceId);

}
