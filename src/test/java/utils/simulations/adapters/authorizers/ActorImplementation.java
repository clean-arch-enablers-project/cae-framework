package utils.simulations.adapters.authorizers;

import com.cae.use_cases.contexts.actors.Actor;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Getter
@Builder
public class ActorImplementation implements Actor<Long> {

    private final Long id;
    private final List<String> scopes;

}
