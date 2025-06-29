package utils.simulations.core.use_cases.update_user;

import com.cae.autofeatures.autoauth.annotations.ScopeBasedProtection;
import com.cae.use_cases.ConsumerUseCase;
import utils.simulations.core.use_cases.update_user.io.inputs.UpdateUserUseCaseInput;

@ScopeBasedProtection(scope = "update:user")
public abstract class UpdateUserUseCase extends ConsumerUseCase<UpdateUserUseCaseInput> { }






