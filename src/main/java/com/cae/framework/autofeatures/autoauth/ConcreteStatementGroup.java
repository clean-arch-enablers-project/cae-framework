package com.cae.framework.autofeatures.autoauth;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteStatementGroup implements StatementGroupContract{

    private String id;
    private Boolean shared;
    private List<StatementContract> statements;

    @Override
    public boolean isShared() {
        return this.shared;
    }

}
