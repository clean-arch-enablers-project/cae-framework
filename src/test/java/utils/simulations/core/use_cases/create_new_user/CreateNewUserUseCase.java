package utils.simulations.core.use_cases.create_new_user;

import com.cae.autofeatures.autoauth.annotations.ScopeBasedProtection;
import com.cae.use_cases.FunctionUseCase;
import utils.simulations.core.use_cases.create_new_user.io.inputs.CreateNewUserUseCaseInput;
import utils.simulations.core.use_cases.create_new_user.io.outputs.CreateNewUserUseCaseOutput;

@ScopeBasedProtection(scope = "create:user")
public abstract class CreateNewUserUseCase extends FunctionUseCase<CreateNewUserUseCaseInput, CreateNewUserUseCaseOutput> {
}
