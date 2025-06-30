package utils.simulations.assemblers.autonotify;

import com.cae.autofeatures.autonotify.AutonotifyProvider;
import com.cae.http_client.implementations.exceptions.IORuntimeException;
import lombok.NoArgsConstructor;
import utils.simulations.assemblers.autonotify.observers.SubscriberOne;
import utils.simulations.assemblers.autonotify.observers.SubscriberTwo;

@NoArgsConstructor
public class MyAppAutonotifyBootstrap {

    public static void startupDefaultSettings(){
        AutonotifyProvider.SINGLETON
                .considerInputMappedExceptions()
                .considerInternalMappedExceptions()
                .considerNotFoundMappedExceptions()
                .considerSpecifically(IORuntimeException.class)
                .considerLatency(3000)
                .setSubscriber(new SubscriberOne())
                .setSubscriber(new SubscriberTwo());
    }

}
