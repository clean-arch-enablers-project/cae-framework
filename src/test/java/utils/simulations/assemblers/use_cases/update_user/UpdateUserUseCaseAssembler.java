package utils.simulations.assemblers.use_cases.update_user;

import com.cae.use_cases.assemblers.UseCaseAssembler;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import utils.simulations.adapters.use_cases.update_user.FetchUserByIdPortAdapter;
import utils.simulations.adapters.use_cases.update_user.SaveUpdatePortAdapter;
import utils.simulations.core.use_cases.update_user.UpdateUserUseCase;
import utils.simulations.core.use_cases.update_user.factory.UpdateUserUseCaseFactory;
import utils.simulations.core.use_cases.update_user.factory.dependency_wrapper.UpdateUserUseCaseDependencyWrapper;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UpdateUserUseCaseAssembler implements UseCaseAssembler<UpdateUserUseCase> {

    public static final UpdateUserUseCaseAssembler INSTANCE = new UpdateUserUseCaseAssembler();

    public static final UpdateUserUseCase V1;

    static {
        V1 = initializeV1();
    }

    private static UpdateUserUseCase initializeV1() {
        var dependencyWrapper = UpdateUserUseCaseDependencyWrapper.builder()
                .fetchUserByIdPortAdapter(new FetchUserByIdPortAdapter())
                .saveUpdatePortAdapter(new SaveUpdatePortAdapter())
                .build();
        return UpdateUserUseCaseFactory.INSTANCE.getOrCreateDefaultSingletonInstance(dependencyWrapper);
    }

    @Override
    public UpdateUserUseCase getDefaultAssembledInstance() {
        return V1;
    }
}
