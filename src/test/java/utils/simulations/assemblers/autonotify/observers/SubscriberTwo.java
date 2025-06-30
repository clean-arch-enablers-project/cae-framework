package utils.simulations.assemblers.autonotify.observers;

import com.cae.autofeatures.autonotify.Notification;
import com.cae.autofeatures.autonotify.AutonotifySubscriber;

public class SubscriberTwo implements AutonotifySubscriber {

    @Override
    public void receiveNotification(Notification notification) {
        System.out.println("ihu: " + notification + " | " + Thread.currentThread().getName());
    }

}
