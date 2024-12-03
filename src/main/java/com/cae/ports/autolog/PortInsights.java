package com.cae.ports.autolog;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PortInsights {

    public static final PortInsights SINGLETON = new PortInsights();

    public final Map<String, List<String>> insightsByCorrelationId = new ConcurrentHashMap<>();

    public List<String> getFor(ExecutionContext context) {
        return this.insightsByCorrelationId.get(context.getCorrelationId().toString());
    }

    public void register(ExecutionContext context, String insight) {
        var insightList = this.getFor(context);
        if (insightList == null){
            insightList = new ArrayList<>();
            this.insightsByCorrelationId.put(context.getCorrelationId().toString(), insightList);
        }
        insightList.add(insight);
    }

    public void flush(ExecutionContext context){
        Optional.ofNullable(this.getFor(context)).ifPresent(value -> this.insightsByCorrelationId.remove(context.getCorrelationId().toString()));
    }

}
