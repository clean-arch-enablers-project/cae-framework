package utils.simulations.core.use_cases.delete_inactive_users.factory;

import com.cae.use_cases.factories.UseCaseFactory;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.delete_inactive_users.DeleteInactiveUsersUseCase;
import utils.simulations.core.use_cases.delete_inactive_users.factory.dependency_wrapper.DeleteInactiveUsersUseCaseDependencyWrapper;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.DeleteInactiveUsersUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DeleteInactiveUsersUseCaseFactory extends UseCaseFactory<DeleteInactiveUsersUseCase, DeleteInactiveUsersUseCaseDependencyWrapper> {

    public static final DeleteInactiveUsersUseCaseFactory INSTANCE = new DeleteInactiveUsersUseCaseFactory();

    @Override
    protected DeleteInactiveUsersUseCase initializeSingletonUsing(DeleteInactiveUsersUseCaseDependencyWrapper dependencyWrapper) {
        return new DeleteInactiveUsersUseCaseImplementation(
                dependencyWrapper.getFetchInactiveUsersPortAdapter(),
                dependencyWrapper.getDeleteUserPortAdapter()
        );
    }
}
