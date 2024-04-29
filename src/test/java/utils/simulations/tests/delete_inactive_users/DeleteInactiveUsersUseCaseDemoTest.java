package utils.simulations.tests.delete_inactive_users;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;
import utils.simulations.assemblers.use_cases.delete_inactive_users.DeleteInactiveUsersUseCaseAssembler;
import utils.simulations.core.use_cases.delete_inactive_users.DeleteInactiveUsersUseCase;

@ExtendWith(MockitoExtension.class)
class DeleteInactiveUsersUseCaseDemoTest {

    private static final DeleteInactiveUsersUseCase USE_CASE = DeleteInactiveUsersUseCaseAssembler.INSTANCE.getDefaultAssembledInstance();

    @BeforeAll
    static void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void runRunnableUseCase() {
        Assertions.assertDoesNotThrow(() -> USE_CASE.execute(UseCaseExecutionCorrelation.ofNew()));
    }

}
