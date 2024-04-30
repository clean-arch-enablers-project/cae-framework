package utils.simulations.adapters.authorizers;

import com.cae.use_cases.authorization.UseCaseExecutionAuthorizer;
import com.cae.use_cases.correlations.actors.Actor;

import java.util.stream.Stream;

public class AuthorizerAdapter implements UseCaseExecutionAuthorizer {

    @Override
    public boolean allows(Actor actor, String[] requiredScopes) {
        var providedScopes = ((ActorImplementation) actor).getScopes();
        return Stream.of(requiredScopes).allMatch(requiredScope -> providedScopes.stream().anyMatch(providedScope -> providedScope.equals(requiredScope)));
    }
}
