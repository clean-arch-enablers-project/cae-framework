package com.cae.use_cases.autoauth;

import java.util.List;

public interface RoleRetriever {

    List<Role> getRolesBy(String actorId);

}
