package utils.simulations.core.use_cases.create_new_user.io.inputs;

import com.cae.autolog.native_io_extraction_mode.json.sensitive.Sensitive;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.io.annotations.NotBlankInputField;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class CreateNewUserUseCaseInput extends UseCaseInput {

    @NotNullInputField
    @NotBlankInputField
    private String name;

    @NotNullInputField
    @NotBlankInputField
    private String username;

    @Sensitive
    @NotNullInputField
    @NotBlankInputField
    private String pass;

    @NotNullInputField
    @NotBlankInputField
    @Sensitive(unmaskedAmount = 5)
    private String legalId;

    @NotNullInputField
    private Integer userTypeCode;

}
