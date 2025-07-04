package com.cae.use_cases;

import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;

import java.util.Optional;

public interface UseCaseWithInput {

    Optional<ResourceOwnershipRetriever> getResourceOwnershipRetriever();

}
