package utils.simulations.tests.update_user;

import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.adapters.authorizers.ActorImplementation;
import utils.simulations.assemblers.autonotify.MyAppAutonotifyBootstrap;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;
import utils.simulations.assemblers.use_cases.update_user.UpdateUserUseCaseAssembler;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.io.inputs.UpdateUserUseCaseInput;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class UpdateUserUseCaseDemoTest {

    private static final UpdateUserUseCase USE_CASE = UpdateUserUseCaseAssembler.INSTANCE.getDefaultAssembledInstance();

    @BeforeAll
    static void setup(){
        MyAppAutonotifyBootstrap.startupDefaultSettings();
        MyAppAutologBootstrap.startupSyncAllTrueSettingsAndNative();
    }

    @Test
    void runConsumerUseCase() {
        Assertions.assertDoesNotThrow(() -> {
            var actor = ActorImplementation.builder()
                    .scopes(List.of("update:user"))
                    .build();
            var useCaseInput = UpdateUserUseCaseInput.builder()
                    .id(1L)
                    .name("Julucin")
                    .typeCode(2)
                    .build();
            var useCaseExecutionCorrelation = ExecutionContext.ofNew(actor);
            USE_CASE.execute(useCaseInput, useCaseExecutionCorrelation);
        });
    }

}
