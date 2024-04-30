package utils.simulations.core.use_cases.delete_inactive_users.implementation.ports;

import com.cae.ports.specifics.consumers.ConsumerPort;
import utils.simulations.core.entities.UserEntity;

public abstract class DeleteUserPort extends ConsumerPort<UserEntity> {
}
