package utils.simulations.adapters.use_cases.delete_inactive_users;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.DeleteUserPort;

@RequiredArgsConstructor
public class DeleteUserPortAdapter extends DeleteUserPort {

    @Override
    protected void executeLogic(UserEntity input, ExecutionContext correlation) {
        //let's pretend it does what it should
    }
}
