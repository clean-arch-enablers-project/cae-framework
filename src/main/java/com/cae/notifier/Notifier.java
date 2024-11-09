package com.cae.notifier;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

public interface Notifier {

    void emitNotification(Notification notification);

    @Builder
    @Getter
    class Notification{
        private final String message;
        private final ExecutionContext executionContext;
        private final Exception exception;
        private final List<Reason> reasons;
    }

    @Builder
    @Getter
    class Reason{
        private final String name;
    }

}
