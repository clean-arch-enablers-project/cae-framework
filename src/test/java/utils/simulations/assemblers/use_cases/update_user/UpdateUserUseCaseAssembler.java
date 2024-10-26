package utils.simulations.assemblers.use_cases.update_user;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.update_user.FetchUserByIdPortAdapter;
import utils.simulations.adapters.use_cases.update_user.SaveUpdatePortAdapter;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.implementation.UpdateUserUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UpdateUserUseCaseAssembler implements UseCaseAssembler<UpdateUserUseCase> {

    public static final UpdateUserUseCaseAssembler INSTANCE = new UpdateUserUseCaseAssembler();

    public static final UpdateUserUseCase V1 = new UpdateUserUseCaseImplementation(
            new FetchUserByIdPortAdapter(),
            new SaveUpdatePortAdapter()
    );

    @Override
    public UpdateUserUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
