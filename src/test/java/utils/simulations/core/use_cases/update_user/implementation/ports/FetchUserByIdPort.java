package utils.simulations.core.use_cases.update_user.implementation.ports;

import com.cae.ports.FunctionPort;
import utils.simulations.core.entities.UserEntity;

import java.util.Optional;

public abstract class FetchUserByIdPort extends FunctionPort<Long, Optional<UserEntity>> {
}
