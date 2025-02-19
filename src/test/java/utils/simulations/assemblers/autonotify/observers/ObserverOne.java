package utils.simulations.assemblers.autonotify.observers;

import com.cae.autonotify.Notification;
import com.cae.autonotify.NotificationObserver;

public class ObserverOne implements NotificationObserver {

    @Override
    public void getNotified(Notification notification) {
        System.out.println("Iha! " + notification + " | " + Thread.currentThread().getName());
    }
}
