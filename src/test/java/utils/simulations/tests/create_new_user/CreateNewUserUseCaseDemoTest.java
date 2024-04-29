package utils.simulations.tests.create_new_user;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;
import utils.simulations.assemblers.use_cases.create_new_user.CreateNewUserUseCaseAssembler;
import utils.simulations.core.use_cases.create_new_user.CreateNewUserUseCase;
import utils.simulations.core.use_cases.create_new_user.io.inputs.CreateNewUserUseCaseInput;

@ExtendWith(MockitoExtension.class)
class CreateNewUserUseCaseDemoTest {

    private static final CreateNewUserUseCase USE_CASE = CreateNewUserUseCaseAssembler.INSTANCE.getDefaultAssembledInstance();

    @BeforeAll
    static void setup(){
        LoggerBootstrapForTesting.startupSyncAllTrueSettingsAndNative();
    }

    @Test
    void runFunctionUseCase() {
        var useCaseInput = CreateNewUserUseCaseInput.builder()
                .name("Julucinho Jr.")
                .pass("12345678")
                .legalId("111.222.333-44")
                .userTypeCode(1)
                .username("xururu")
                .build();
        var useCaseOutput = USE_CASE.execute(useCaseInput, UseCaseExecutionCorrelation.ofNew());
        Assertions.assertNotNull(useCaseOutput);
    }

}
