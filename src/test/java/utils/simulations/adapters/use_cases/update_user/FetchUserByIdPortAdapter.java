package utils.simulations.adapters.use_cases.update_user;

import com.cae.use_cases.contexts.ExecutionContext;
import utils.simulations.core.entities.CPFEntity;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.entities.enums.UserTypeEnum;
import utils.simulations.core.use_cases.update_user.implementation.ports.FetchUserByIdPort;

import java.util.Optional;

public class FetchUserByIdPortAdapter extends FetchUserByIdPort {

    @Override
    protected Optional<UserEntity> executeLogic(Long input, ExecutionContext correlation) {
        //let's pretend it actually fetched from database
        return Optional.of(this.mockUser(input));
    }

    private UserEntity mockUser(Long input) {
        return UserEntity.builder()
                .name("User Xurupita")
                .username("xurupitaxd")
                .active(true)
                .legalId(CPFEntity.builder().value("111.222.333-44").build())
                .userType(UserTypeEnum.PREMIUM)
                .build();
    }
}
