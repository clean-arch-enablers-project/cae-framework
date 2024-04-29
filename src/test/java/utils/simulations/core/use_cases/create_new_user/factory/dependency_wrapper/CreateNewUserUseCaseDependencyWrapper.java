package utils.simulations.core.use_cases.create_new_user.factory.dependency_wrapper;

import com.cae.use_cases.dependency_wrappers.UseCaseDependencyWrapper;
import lombok.Builder;
import utils.simulations.core.use_cases.create_new_user.implementation.ports.StoreNewUserPort;

@Builder
public class CreateNewUserUseCaseDependencyWrapper extends UseCaseDependencyWrapper {

    private final StoreNewUserPort storeNewUserPortAdapter;

    public StoreNewUserPort getStoreNewUserPortAdapter() {
        return this.getValueWithNullSafety(this.storeNewUserPortAdapter);
    }
}
