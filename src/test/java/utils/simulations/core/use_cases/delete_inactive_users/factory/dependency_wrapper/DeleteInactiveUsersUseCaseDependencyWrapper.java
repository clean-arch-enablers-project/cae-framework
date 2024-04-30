package utils.simulations.core.use_cases.delete_inactive_users.factory.dependency_wrapper;

import com.cae.use_cases.dependency_wrappers.UseCaseDependencyWrapper;
import lombok.Builder;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.DeleteUserPort;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.FetchInactiveUsersPort;

@Builder
public class DeleteInactiveUsersUseCaseDependencyWrapper extends UseCaseDependencyWrapper {

    private final FetchInactiveUsersPort fetchInactiveUsersPortAdapter;
    private final DeleteUserPort deleteUserPortAdapter;

    public FetchInactiveUsersPort getFetchInactiveUsersPortAdapter() {
        return this.getValueWithNullSafety(this.fetchInactiveUsersPortAdapter);
    }

    public DeleteUserPort getDeleteUserPortAdapter() {
        return this.getValueWithNullSafety(this.deleteUserPortAdapter);
    }
}
