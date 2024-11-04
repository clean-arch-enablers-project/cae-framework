package com.cae.use_cases.authorization;

import java.util.Optional;

public interface ResourceOwnershipRetriever {

    Optional<String> findByResourceId(Object resourceId);

}
