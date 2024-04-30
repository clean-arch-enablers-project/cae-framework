package utils.simulations.core.use_cases.create_new_user.implementation;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.entities.CPFEntity;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.entities.enums.UserTypeEnum;
import utils.simulations.core.use_cases.create_new_user.CreateNewUserUseCase;
import utils.simulations.core.use_cases.create_new_user.implementation.ports.StoreNewUserPort;
import utils.simulations.core.use_cases.create_new_user.io.inputs.CreateNewUserUseCaseInput;
import utils.simulations.core.use_cases.create_new_user.io.outputs.CreateNewUserUseCaseOutput;

@RequiredArgsConstructor
public class CreateNewUserUseCaseImplementation extends CreateNewUserUseCase {

    private final StoreNewUserPort storeNewUserPort;

    @Override
    protected CreateNewUserUseCaseOutput applyInternalLogic(
            CreateNewUserUseCaseInput input,
            UseCaseExecutionCorrelation correlation) {
        var cpf = CPFEntity.builder()
                .value(input.getLegalId())
                .build();
        cpf.runValueValidation();
        var newUser = UserEntity.builder()
                .name(input.getName())
                .username(input.getUsername())
                .pass(input.getPass())
                .legalId(cpf)
                .userType(UserTypeEnum.ofCode(input.getUserTypeCode()))
                .build();
        newUser.activate();
        var newUserId = this.storeNewUserPort.executePortOn(newUser, correlation);
        return CreateNewUserUseCaseOutput.builder()
                .idNewUser(newUserId)
                .build();
    }
}
