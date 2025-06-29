package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Builder
@Getter
public class Notification{
    private final String subject;
    private final ExecutionContext executionContext;
    private final Exception exception;
    private final List<String> reasons;

    @Override
    public String toString() {
        return "Notification generated on '" +
                this.subject +"' during the execution of correlation ID '" +
                this.executionContext.toString() + "' because of the following reasons: " +
                SimpleJsonBuilder.buildFor(this.reasons);
    }
}
