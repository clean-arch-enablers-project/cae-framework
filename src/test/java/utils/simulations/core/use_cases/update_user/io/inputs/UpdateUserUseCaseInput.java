package utils.simulations.core.use_cases.update_user.io.inputs;

import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class UpdateUserUseCaseInput extends UseCaseInput {

    @NotNullInputField
    private final Long id;

    @NotBlankInputField
    @NotNullInputField
    private final String name;

    @NotNullInputField
    private final Integer typeCode;

}
