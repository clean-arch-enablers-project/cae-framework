package utils.simulations.assemblers.use_cases.delete_inactive_users;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.delete_inactive_users.DeleteUserPortAdapter;
import utils.simulations.adapters.use_cases.delete_inactive_users.FetchInactiveUsersPortAdapter;
import utils.simulations.core.use_cases.delete_inactive_users.DeleteInactiveUsersUseCase;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.DeleteInactiveUsersUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DeleteInactiveUsersUseCaseAssembler implements UseCaseAssembler<DeleteInactiveUsersUseCase> {

    public static final DeleteInactiveUsersUseCaseAssembler INSTANCE = new DeleteInactiveUsersUseCaseAssembler();

    public static final DeleteInactiveUsersUseCase V1 = new DeleteInactiveUsersUseCaseImplementation(
            new FetchInactiveUsersPortAdapter(),
            new DeleteUserPortAdapter()
    );

    @Override
    public DeleteInactiveUsersUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
