package utils.simulations.core.use_cases.update_user.implementation;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.implementation.ports.FetchUserByIdPort;
import utils.simulations.core.use_cases.update_user.implementation.ports.SaveUpdatePort;
import utils.simulations.core.use_cases.update_user.io.inputs.UpdateUserUseCaseInput;

@RequiredArgsConstructor
public class UpdateUserUseCaseImplementation extends UpdateUserUseCase {

    private final FetchUserByIdPort fetchUserByIdPort;
    private final SaveUpdatePort saveUpdatePort;

    @Override
    protected void applyInternalLogic(UpdateUserUseCaseInput input, UseCaseExecutionCorrelation correlation) {

    }
}
