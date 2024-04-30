package utils.simulations.assemblers.authorizers;

import com.cae.use_cases.authorization.UseCaseExecutionAuthorizerProvider;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.authorizers.AuthorizerAdapter;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AuthorizerBoostrap {

    public static void startupBootstrappingSettings(){
        UseCaseExecutionAuthorizerProvider.SINGLETON
                .setProvidedInstance(new AuthorizerAdapter());
    }

}
