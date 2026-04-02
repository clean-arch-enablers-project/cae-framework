package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autoauth.models.Role;

import java.util.List;

public interface RoleRetriever {

    List<Role> getRolesBy(String actorId, ExecutionContext context);

}
