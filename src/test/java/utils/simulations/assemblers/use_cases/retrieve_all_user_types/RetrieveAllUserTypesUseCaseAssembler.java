package utils.simulations.assemblers.use_cases.retrieve_all_user_types;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCase;
import utils.simulations.core.use_cases.retrieve_all_user_types.factory.RetrieveAllUserTypesUseCaseFactory;
import utils.simulations.core.use_cases.retrieve_all_user_types.factory.dependency_wrapper.RetrieveAllUserTypesUseCaseDependencyWrapper;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RetrieveAllUserTypesUseCaseAssembler implements UseCaseAssembler<RetrieveAllUserTypesUseCase> {

    public static final RetrieveAllUserTypesUseCaseAssembler INSTANCE = new RetrieveAllUserTypesUseCaseAssembler();

    public static final RetrieveAllUserTypesUseCase V1;

    static {
        V1 = initializeV1();
    }

    private static RetrieveAllUserTypesUseCase initializeV1() {
        return RetrieveAllUserTypesUseCaseFactory.INSTANCE.getOrCreateDefaultSingletonInstance(new RetrieveAllUserTypesUseCaseDependencyWrapper());
    }

    @Override
    public RetrieveAllUserTypesUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
