package utils.simulations.core.use_cases.retrieve_all_user_types.io.outputs;

import lombok.Builder;
import lombok.Getter;
import utils.simulations.core.entities.enums.UserTypeEnum;

import java.util.List;

@Builder
@Getter
public class RetrieveAllUserTypesUseCaseOutput {

    private final List<UserTypeEnum> userTypes;

}
