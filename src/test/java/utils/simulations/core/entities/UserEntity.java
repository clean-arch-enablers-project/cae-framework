package utils.simulations.core.entities;

import com.cae.autolog.native_io_extraction_mode.json.sensitive.Sensitive;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import utils.simulations.core.entities.enums.UserTypeEnum;

@Builder
@Getter
@Setter
public class UserEntity {

    private Long id;
    private String name;
    private String username;
    private Boolean active;
    @Sensitive
    private String pass;
    private CPFEntity legalId;
    private UserTypeEnum userType;

    public void activate(){
        this.active = true;
    }

}
