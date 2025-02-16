package com.cae.autonotify;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

public interface Notifier {

    void emitNotification(Notification notification);

    @Builder
    @Getter
    class Notification{
        private final String subject;
        private final ExecutionContext executionContext;
        private final Exception exception;
        private final List<String> reasons;
    }

}
