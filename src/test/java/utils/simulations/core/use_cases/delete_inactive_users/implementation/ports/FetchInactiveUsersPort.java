package utils.simulations.core.use_cases.delete_inactive_users.implementation.ports;

import com.cae.ports.SupplierPort;
import utils.simulations.core.entities.UserEntity;

import java.util.List;

public abstract class FetchInactiveUsersPort extends SupplierPort<List<UserEntity>> {
}
