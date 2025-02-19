package utils.simulations.tests.retrieve_all_user_types;

import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.autonotify.MyAppAutonotifyBootstrap;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;
import utils.simulations.assemblers.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCaseAssembler;
import utils.simulations.core.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCase;

@ExtendWith(MockitoExtension.class)
class RetrieveAllUserTypesUseCaseDemoTest {

    private static final RetrieveAllUserTypesUseCase USE_CASE = RetrieveAllUserTypesUseCaseAssembler.INSTANCE.getDefaultAssembledInstance();

    @BeforeAll
    static void setup(){
        MyAppAutonotifyBootstrap.startupDefaultSettings();
        MyAppAutologBootstrap.startupSyncAllTrueSettingsAndNative();
    }

    @Test
    void runSupplierUseCase(){
        var useCaseOutput = USE_CASE.execute(ExecutionContext.ofNew());
        Assertions.assertNotNull(useCaseOutput);
    }

}
