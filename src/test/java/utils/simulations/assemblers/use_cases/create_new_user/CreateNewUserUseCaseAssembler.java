package utils.simulations.assemblers.use_cases.create_new_user;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.create_new_user.StoreNewUserPortAdapter;
import utils.simulations.core.use_cases.create_new_user.CreateNewUserUseCase;
import utils.simulations.core.use_cases.create_new_user.factory.CreateNewUserUseCaseFactory;
import utils.simulations.core.use_cases.create_new_user.factory.dependency_wrapper.CreateNewUserUseCaseDependencyWrapper;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CreateNewUserUseCaseAssembler implements UseCaseAssembler<CreateNewUserUseCase> {

    public static final CreateNewUserUseCaseAssembler INSTANCE = new CreateNewUserUseCaseAssembler();

    public static final CreateNewUserUseCase V1;

    static {
        V1 = initializeV1();
    }

    private static CreateNewUserUseCase initializeV1() {
        var dependencyWrapper = CreateNewUserUseCaseDependencyWrapper
                .builder()
                .storeNewUserPortAdapter(new StoreNewUserPortAdapter())
                .build();
        return CreateNewUserUseCaseFactory.INSTANCE.getOrCreateDefaultSingletonInstance(dependencyWrapper);
    }

    @Override
    public CreateNewUserUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
