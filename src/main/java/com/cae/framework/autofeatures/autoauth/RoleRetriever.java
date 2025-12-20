package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;

import java.util.List;

public interface RoleRetriever {

    List<RoleContract> getRolesBy(String actorId, ExecutionContext context);

}
