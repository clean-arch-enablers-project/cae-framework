package utils.simulations.adapters.use_cases.update_user;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.use_cases.update_user.implementation.ports.SaveUpdatePort;

public class SaveUpdatePortAdapter extends SaveUpdatePort {

    @Override
    protected void executeLogic(UserEntity input, UseCaseExecutionCorrelation correlation) {
        //let's pretend it did what it should
    }
}
