package utils.simulations.adapters.use_cases.update_user;

import com.cae.use_cases.contexts.ExecutionContext;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.use_cases.update_user.implementation.ports.SaveUpdatePort;

public class SaveUpdatePortAdapter extends SaveUpdatePort {

    @Override
    protected void executeLogic(UserEntity input, ExecutionContext correlation) {
        //let's pretend it did what it should
    }
}
