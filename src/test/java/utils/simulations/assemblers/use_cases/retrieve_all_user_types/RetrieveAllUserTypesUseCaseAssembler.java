package utils.simulations.assemblers.use_cases.retrieve_all_user_types;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCase;
import utils.simulations.core.use_cases.retrieve_all_user_types.implementation.RetrieveAllUserTypesUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RetrieveAllUserTypesUseCaseAssembler implements UseCaseAssembler<RetrieveAllUserTypesUseCase> {

    public static final RetrieveAllUserTypesUseCaseAssembler INSTANCE = new RetrieveAllUserTypesUseCaseAssembler();

    public static final RetrieveAllUserTypesUseCase V1 = new RetrieveAllUserTypesUseCaseImplementation();

    @Override
    public RetrieveAllUserTypesUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
