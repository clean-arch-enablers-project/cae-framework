package utils.simulations.core.use_cases.retrieve_all_user_types.factory;

import com.cae.use_cases.factories.UseCaseFactory;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.core.use_cases.retrieve_all_user_types.RetrieveAllUserTypesUseCase;
import utils.simulations.core.use_cases.retrieve_all_user_types.factory.dependency_wrapper.RetrieveAllUserTypesUseCaseDependencyWrapper;
import utils.simulations.core.use_cases.retrieve_all_user_types.implementation.RetrieveAllUserTypesUseCaseImplementation;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RetrieveAllUserTypesUseCaseFactory extends UseCaseFactory<RetrieveAllUserTypesUseCase, RetrieveAllUserTypesUseCaseDependencyWrapper> {

    public static final RetrieveAllUserTypesUseCaseFactory INSTANCE = new RetrieveAllUserTypesUseCaseFactory();

    @Override
    protected RetrieveAllUserTypesUseCase initializeSingletonUsing(RetrieveAllUserTypesUseCaseDependencyWrapper dependencyWrapper) {
        return new RetrieveAllUserTypesUseCaseImplementation();
    }
}
