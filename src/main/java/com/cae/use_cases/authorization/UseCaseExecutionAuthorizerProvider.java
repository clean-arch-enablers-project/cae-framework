package com.cae.use_cases.authorization;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Optional;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Setter
public class UseCaseExecutionAuthorizerProvider {

    public static final UseCaseExecutionAuthorizerProvider SINGLETON = new UseCaseExecutionAuthorizerProvider();

    private UseCaseExecutionAuthorizer providedInstance;

    public Optional<UseCaseExecutionAuthorizer> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }


}
