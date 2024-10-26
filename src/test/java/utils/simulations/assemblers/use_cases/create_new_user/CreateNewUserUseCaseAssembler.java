package utils.simulations.assemblers.use_cases.create_new_user;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.create_new_user.StoreNewUserPortAdapter;
import utils.simulations.core.use_cases.create_new_user.CreateNewUserUseCase;
import utils.simulations.core.use_cases.create_new_user.implementation.CreateNewUserUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CreateNewUserUseCaseAssembler implements UseCaseAssembler<CreateNewUserUseCase> {

    public static final CreateNewUserUseCaseAssembler INSTANCE = new CreateNewUserUseCaseAssembler();

    public static final CreateNewUserUseCase V1 = new CreateNewUserUseCaseImplementation(new StoreNewUserPortAdapter());

    @Override
    public CreateNewUserUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
