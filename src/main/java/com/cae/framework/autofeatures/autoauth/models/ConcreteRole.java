package com.cae.framework.autofeatures.autoauth.models;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ConcreteRole implements Role {

    private String id;
    private String ownerId;
    private Policy policy;

}
