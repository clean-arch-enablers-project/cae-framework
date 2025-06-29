package com.cae.use_cases.registries;

import com.cae.use_cases.UseCase;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseRegistry {

    public static final UseCaseRegistry SINGLETON = new UseCaseRegistry();

    public final List<UseCaseRegistryInterested> interestedParties = new ArrayList<>();
    private final List<UseCase> useCases = new ArrayList<>();

    public void add(UseCase useCase){
        this.useCases.add(useCase);
        this.interestedParties.forEach(interested -> interested.notifyNewUseCaseRegistered(useCase));
    }

    public List<UseCase> getRegisteredUseCases(){
        return this.useCases;
    }

    public UseCaseRegistry registerInterest(UseCaseRegistryInterested interested){
        this.interestedParties.add(interested);
        return this;
    }

    public interface UseCaseRegistryInterested{

        void notifyNewUseCaseRegistered(UseCase newUseCase);

    }

}
