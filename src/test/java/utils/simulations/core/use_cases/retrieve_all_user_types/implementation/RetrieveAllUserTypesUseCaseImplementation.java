package utils.simulations.core.use_cases.retrieve_all_user_types.implementation;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.entities.enums.UserTypeEnum;
import utils.simulations.core.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCase;
import utils.simulations.core.use_cases.retrieve_all_user_types.io.outputs.RetrieveAllUserTypesUseCaseOutput;

import java.util.List;

@RequiredArgsConstructor
public class RetrieveAllUserTypesUseCaseImplementation extends RetrieveAllUserTypesUseCase {

    @Override
    protected RetrieveAllUserTypesUseCaseOutput applyInternalLogic(UseCaseExecutionCorrelation useCaseExecutionCorrelation) {
        return RetrieveAllUserTypesUseCaseOutput.builder()
                .userTypes(List.of(UserTypeEnum.values()))
                .build();
    }



}
