package com.cae.framework.autofeatures.autoauth.models;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteCaeRole implements CaeRole {

    private String id;
    private String ownerId;
    private CaePolicy policy;

}
