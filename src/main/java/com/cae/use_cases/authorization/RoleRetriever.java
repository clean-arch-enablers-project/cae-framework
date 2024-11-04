package com.cae.use_cases.authorization;

import java.util.List;

public interface RoleRetriever {

    List<Role> getRolesBy(String actorId);

}
