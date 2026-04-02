package com.cae.framework.autofeatures.autoauth.models;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteStatement implements Statement {

    private String id;
    private Boolean allows;
    private List<String> actionIds;

    @Override
    public boolean allows() {
        return this.allows;
    }
}
