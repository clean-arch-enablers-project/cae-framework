package com.cae.framework.use_cases;

import com.cae.framework.autofeatures.autoauth.ResourceOwnershipRetriever;

import java.util.Optional;

public interface UseCaseWithInput {

    Optional<ResourceOwnershipRetriever> getResourceOwnershipRetriever();

}
