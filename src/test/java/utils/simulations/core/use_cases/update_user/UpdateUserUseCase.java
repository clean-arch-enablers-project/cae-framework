package utils.simulations.core.use_cases.update_user;

import com.cae.use_cases.authorization.annotations.ScopeBasedProtectedUseCase;
import com.cae.use_cases.specifics.consumers.ConsumerUseCase;
import utils.simulations.core.use_cases.update_user.io.inputs.UpdateUserUseCaseInput;

@ScopeBasedProtectedUseCase(scope = "update:user")
public abstract class UpdateUserUseCase extends ConsumerUseCase<UpdateUserUseCaseInput> { }






