package utils.simulations.core.use_cases.create_new_user.io.outputs;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class CreateNewUserUseCaseOutput {

    private final Long idNewUser;

}
