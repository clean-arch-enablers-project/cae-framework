package utils.simulations.core.use_cases.delete_inactive_users;

import com.cae.use_cases.authorization.annotations.ScopeBasedProtectedUseCase;
import com.cae.use_cases.specifics.runnables.RunnableUseCase;

@ScopeBasedProtectedUseCase(scope = "delete:user")
public abstract class DeleteInactiveUsersUseCase extends RunnableUseCase { }


