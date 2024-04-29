package utils.simulations.core.use_cases.update_user.factory.dependency_wrapper;

import com.cae.use_cases.dependency_wrappers.UseCaseDependencyWrapper;
import lombok.Builder;
import utils.simulations.core.use_cases.update_user.implementation.ports.FetchUserByIdPort;
import utils.simulations.core.use_cases.update_user.implementation.ports.SaveUpdatePort;

@Builder
public class UpdateUserUseCaseDependencyWrapper extends UseCaseDependencyWrapper {

    private final FetchUserByIdPort fetchUserByIdPortAdapter;
    private final SaveUpdatePort saveUpdatePortAdapter;

    public FetchUserByIdPort getFetchUserByIdPortAdapter() {
        return this.getValueWithNullSafety(this.fetchUserByIdPortAdapter);
    }

    public SaveUpdatePort getSaveUpdatePortAdapter() {
        return this.getValueWithNullSafety(this.saveUpdatePortAdapter);
    }
}
