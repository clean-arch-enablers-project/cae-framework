package utils.simulations.core.use_cases.delete_inactive_users.implementation;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.use_cases.delete_inactive_users.DeleteInactiveUsersUseCase;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.DeleteUserPort;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.FetchInactiveUsersPort;

@RequiredArgsConstructor
public class DeleteInactiveUsersUseCaseImplementation extends DeleteInactiveUsersUseCase {

    private final FetchInactiveUsersPort fetchInactiveUsersPort;
    private final DeleteUserPort deleteUserPort;

    @Override
    protected void applyInternalLogic(ExecutionContext context) {
        var inactiveUsers = this.fetchInactiveUsersPort.executePort(context);
        inactiveUsers.forEach(inactiveUser -> this.deleteUserPort.executePortOn(inactiveUser, context));
    }
}
