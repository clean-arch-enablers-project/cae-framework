package utils.simulations.assemblers.autonotify;

import com.cae.autonotify.AutonotifyProvider;
import com.cae.http_client.implementations.exceptions.IORuntimeException;
import lombok.NoArgsConstructor;
import utils.simulations.assemblers.autonotify.observers.ObserverOne;
import utils.simulations.assemblers.autonotify.observers.ObserverTwo;

@NoArgsConstructor
public class MyAppAutonotifyBootstrap {

    public static void startupDefaultSettings(){
        AutonotifyProvider.SINGLETON
                .considerInputMappedExceptions()
                .considerInternalMappedExceptions()
                .considerNotFoundMappedExceptions()
                .considerSpecifically(IORuntimeException.class)
                .considerLatency(3000)
                .setProvidedInstance(new ObserverOne())
                .setProvidedInstance(new ObserverTwo());
    }

}
