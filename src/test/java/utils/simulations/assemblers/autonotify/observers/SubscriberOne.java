package utils.simulations.assemblers.autonotify.observers;

import com.cae.autofeatures.autonotify.Notification;
import com.cae.autofeatures.autonotify.AutonotifySubscriber;

public class SubscriberOne implements AutonotifySubscriber {

    @Override
    public void receiveNotification(Notification notification) {
        System.out.println("Iha! " + notification + " | " + Thread.currentThread().getName());
    }
}
