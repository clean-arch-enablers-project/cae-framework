package utils.simulations.core.entities;

import com.cae.autofeatures.autolog.native_io_extraction_mode.json.sensitive.Sensitive;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class CPFEntity {

    @Sensitive(unmaskedAmount = 5)
    private String value;

    public void runValueValidation(){
        //let's pretend it does something here.
    }

}
