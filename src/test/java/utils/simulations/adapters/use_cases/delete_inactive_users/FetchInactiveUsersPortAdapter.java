package utils.simulations.adapters.use_cases.delete_inactive_users;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.RequiredArgsConstructor;
import utils.simulations.core.entities.UserEntity;
import utils.simulations.core.use_cases.delete_inactive_users.implementation.ports.FetchInactiveUsersPort;

import java.util.List;

@RequiredArgsConstructor
public class FetchInactiveUsersPortAdapter extends FetchInactiveUsersPort {

    @Override
    protected List<UserEntity> executeLogic(UseCaseExecutionCorrelation correlation) {
        //let's pretend it actually fetched them from database
        return List.of(
                this.mockUser1(),
                this.mockUser2(),
                this.mockUser3(),
                this.mockUser4()
        );
    }

    private UserEntity mockUser1() {
        return UserEntity.builder().id(44L).active(false).build();
    }

    private UserEntity mockUser2() {
        return UserEntity.builder().id(45847L).active(false).build();
    }

    private UserEntity mockUser3() {
        return UserEntity.builder().id(1959L).active(false).build();
    }

    private UserEntity mockUser4() {
        return UserEntity.builder().id(1982L).active(false).build();
    }
}
