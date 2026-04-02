package com.cae.framework.autofeatures.autoauth.models;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcretePolicy implements Policy {

    private String id;
    private Boolean automanaged;
    private List<Statement> statements;

    @Override
    public boolean isAutomanaged() {
        return this.automanaged;
    }

}
