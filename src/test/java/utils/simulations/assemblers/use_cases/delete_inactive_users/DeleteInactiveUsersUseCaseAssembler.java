package utils.simulations.assemblers.use_cases.delete_inactive_users;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.delete_inactive_users.DeleteUserPortAdapter;
import utils.simulations.adapters.use_cases.delete_inactive_users.FetchInactiveUsersPortAdapter;
import utils.simulations.core.use_cases.delete_inactive_users.DeleteInactiveUsersUseCase;
import utils.simulations.core.use_cases.delete_inactive_users.factory.DeleteInactiveUsersUseCaseFactory;
import utils.simulations.core.use_cases.delete_inactive_users.factory.dependency_wrapper.DeleteInactiveUsersUseCaseDependencyWrapper;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DeleteInactiveUsersUseCaseAssembler implements UseCaseAssembler<DeleteInactiveUsersUseCase> {

    public static final DeleteInactiveUsersUseCaseAssembler INSTANCE = new DeleteInactiveUsersUseCaseAssembler();

    public static final DeleteInactiveUsersUseCase V1;

    static {
        V1 = initializeV1();
    }

    private static DeleteInactiveUsersUseCase initializeV1() {
        var dependencyWrapper = DeleteInactiveUsersUseCaseDependencyWrapper
                .builder()
                .fetchInactiveUsersPortAdapter(new FetchInactiveUsersPortAdapter())
                .deleteUserPortAdapter(new DeleteUserPortAdapter())
                .build();
        return DeleteInactiveUsersUseCaseFactory.INSTANCE.getOrCreateDefaultSingletonInstance(dependencyWrapper);
    }

    @Override
    public DeleteInactiveUsersUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
