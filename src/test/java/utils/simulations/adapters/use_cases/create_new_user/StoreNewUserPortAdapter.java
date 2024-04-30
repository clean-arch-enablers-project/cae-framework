package utils.simulations.adapters.use_cases.create_new_user;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.use_cases.create_new_user.implementation.ports.StoreNewUserPort;

@RequiredArgsConstructor
public class StoreNewUserPortAdapter extends StoreNewUserPort {

    @Override
    protected Long executeLogic(UserEntity input, UseCaseExecutionCorrelation correlation) {
        //let's pretend it actually saves and returns 1
        return 1L;
    }
}
