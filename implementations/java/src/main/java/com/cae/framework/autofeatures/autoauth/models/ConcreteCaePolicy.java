package com.cae.framework.autofeatures.autoauth.models;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteCaePolicy implements CaePolicy {

    private String id;
    private Boolean automanaged;
    private List<CaeStatement> statements;

    @Override
    public boolean isAutomanaged() {
        return this.automanaged;
    }

}
