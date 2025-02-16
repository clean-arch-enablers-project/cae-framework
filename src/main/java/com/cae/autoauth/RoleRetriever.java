package com.cae.autoauth;

import com.cae.use_cases.contexts.ExecutionContext;

import java.util.List;

public interface RoleRetriever {

    List<RoleContract> getRolesBy(String actorId, ExecutionContext context);

}
