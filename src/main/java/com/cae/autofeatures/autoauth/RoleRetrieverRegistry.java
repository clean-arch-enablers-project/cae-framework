package com.cae.autofeatures.autoauth;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RoleRetrieverRegistry {

    public static final RoleRetrieverRegistry SINGLETON = new RoleRetrieverRegistry();

    private Map<String, RoleRetriever> roleRetrieverByUseCaseId = new HashMap<>();
    private RoleRetriever defaultRoleRetriever;

    public RoleRetrieverRegistry setDefaultRoleRetriever(RoleRetriever roleRetriever){
        this.defaultRoleRetriever = roleRetriever;
        return this;
    }

    public Optional<RoleRetriever> getDefaultRetriever(){
        return Optional.ofNullable(this.defaultRoleRetriever);
    }

    public RoleRetrieverRegistry putRetrieverByUseCaseId(RoleRetriever roleRetriever, String useCaseId){
        this.roleRetrieverByUseCaseId.put(useCaseId, roleRetriever);
        return this;
    }

    public Optional<RoleRetriever> getRoleRetrieverByUseCaseId(String useCaseId){
        return Optional.ofNullable(this.roleRetrieverByUseCaseId.get(useCaseId));
    }

    public void reset(){
        this.roleRetrieverByUseCaseId = new HashMap<>();
        this.defaultRoleRetriever = null;
    }

}
