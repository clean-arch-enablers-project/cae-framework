package utils.simulations.assemblers.autonotify.observers;

import com.cae.autonotify.Notification;
import com.cae.autonotify.NotificationObserver;

public class ObserverTwo implements NotificationObserver {

    @Override
    public void getNotified(Notification notification) {
        System.out.println("ihu: " + notification + " | " + Thread.currentThread().getName());
    }

}
