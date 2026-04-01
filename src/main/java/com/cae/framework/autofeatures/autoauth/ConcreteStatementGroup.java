package com.cae.framework.autofeatures.autoauth;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConcreteStatementGroup implements StatementGroup {

    private String id;
    private Boolean shared;
    private List<Statement> statements;

    @Override
    public boolean isShared() {
        return this.shared;
    }

}
