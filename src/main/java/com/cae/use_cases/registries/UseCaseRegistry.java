package com.cae.use_cases.registries;

import com.cae.use_cases.UseCase;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseRegistry {

    public static final UseCaseRegistry SINGLETON = new UseCaseRegistry(new ArrayList<>());

    private final List<UseCase> useCases;

    public void add(UseCase useCase){
        this.useCases.add(useCase);
    }

    public List<UseCase> getRegisteredUseCases(){
        return this.useCases;
    }

}
