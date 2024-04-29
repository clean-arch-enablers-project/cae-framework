package utils.simulations.core.use_cases.update_user.factory;

import com.cae.use_cases.factories.UseCaseFactory;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.factory.dependency_wrapper.UpdateUserUseCaseDependencyWrapper;
import utils.simulations.core.use_cases.update_user.implementation.UpdateUserUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UpdateUserUseCaseFactory extends UseCaseFactory<UpdateUserUseCase, UpdateUserUseCaseDependencyWrapper> {

    public static final UpdateUserUseCaseFactory INSTANCE = new UpdateUserUseCaseFactory();

    @Override
    protected UpdateUserUseCase initializeSingletonUsing(UpdateUserUseCaseDependencyWrapper dependencyWrapper) {
        return new UpdateUserUseCaseImplementation(
                dependencyWrapper.getFetchUserByIdPortAdapter(),
                dependencyWrapper.getSaveUpdatePortAdapter()
        );
    }
}
