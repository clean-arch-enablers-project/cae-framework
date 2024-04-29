package utils.simulations.tests.update_user;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;
import utils.simulations.assemblers.use_cases.update_user.UpdateUserUseCaseAssembler;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.io.inputs.UpdateUserUseCaseInput;

@ExtendWith(MockitoExtension.class)
class UpdateUserUseCaseDemoTest {

    private static final UpdateUserUseCase USE_CASE = UpdateUserUseCaseAssembler.INSTANCE.getDefaultAssembledInstance();

    @BeforeAll
    static void setup(){
        LoggerBootstrapForTesting.startupSyncAllTrueSettingsAndNative();
    }

    @Test
    void runConsumerUseCase() {
        Assertions.assertDoesNotThrow(() -> {
            var useCaseInput = UpdateUserUseCaseInput.builder()
                    .id(1L)
                    .name("Julucin")
                    .typeCode(2)
                    .build();
            var useCaseExecutionCorrelation = UseCaseExecutionCorrelation.ofNew();
            USE_CASE.execute(useCaseInput, useCaseExecutionCorrelation);
        });
    }

}
