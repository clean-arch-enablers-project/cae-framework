package utils.simulations.core.entities.enums;

import com.cae.mapped_exceptions.specifics.NotFoundMappedException;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.stream.Stream;

@RequiredArgsConstructor
@Getter
public enum UserTypeEnum {

    PREMIUM(1, "Premium"),
    STANDARD(2, "Standard");

    private final Integer code;
    private final String name;

    public static UserTypeEnum ofCode(Integer userTypeCode) {
        return Stream.of(UserTypeEnum.values())
                .filter(value -> value.getCode().equals(userTypeCode))
                .findFirst()
                .orElseThrow(() -> new NotFoundMappedException("No user type of code " + userTypeCode));
    }
}
