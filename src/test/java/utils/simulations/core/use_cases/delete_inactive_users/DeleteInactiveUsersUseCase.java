package utils.simulations.core.use_cases.delete_inactive_users;

import com.cae.use_cases.autoauth.annotations.ScopeBasedProtection;
import com.cae.use_cases.RunnableUseCase;

@ScopeBasedProtection(scope = "delete:user")
public abstract class DeleteInactiveUsersUseCase extends RunnableUseCase { }


