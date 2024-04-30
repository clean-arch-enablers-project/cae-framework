package utils.simulations.core.use_cases.create_new_user.factory;

import com.cae.use_cases.factories.UseCaseFactory;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.create_new_user.CreateNewUserUseCase;
import utils.simulations.core.use_cases.create_new_user.factory.dependency_wrapper.CreateNewUserUseCaseDependencyWrapper;
import utils.simulations.core.use_cases.create_new_user.implementation.CreateNewUserUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CreateNewUserUseCaseFactory extends UseCaseFactory<CreateNewUserUseCase, CreateNewUserUseCaseDependencyWrapper> {

    public static final CreateNewUserUseCaseFactory INSTANCE = new CreateNewUserUseCaseFactory();

    @Override
    protected CreateNewUserUseCase initializeSingletonUsing(CreateNewUserUseCaseDependencyWrapper dependencyWrapper) {
        return new CreateNewUserUseCaseImplementation(
                dependencyWrapper.getStoreNewUserPortAdapter()
        );
    }
}
