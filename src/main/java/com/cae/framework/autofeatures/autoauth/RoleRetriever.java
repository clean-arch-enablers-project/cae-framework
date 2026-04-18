package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autoauth.models.CaeRole;

import java.util.List;

public interface RoleRetriever {

    List<CaeRole> getRolesBy(String actorId, String actionId, ExecutionContext context);

}
