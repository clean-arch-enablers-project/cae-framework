package utils.simulations.core.use_cases.create_new_user;

import com.cae.use_cases.authorization.annotations.ScopeBasedProtectedUseCase;
import com.cae.use_cases.specifics.functions.FunctionUseCase;
import utils.simulations.core.use_cases.create_new_user.io.inputs.CreateNewUserUseCaseInput;
import utils.simulations.core.use_cases.create_new_user.io.outputs.CreateNewUserUseCaseOutput;

@ScopeBasedProtectedUseCase(scope = "create:user")
public abstract class CreateNewUserUseCase extends FunctionUseCase<CreateNewUserUseCaseInput, CreateNewUserUseCaseOutput> {
}
