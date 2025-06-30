package com.cae.autofeatures.autodoc.components;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class Responsible {

    public static Responsible of(String name, String email){
       return Responsible.builder()
               .name(name)
               .email(email)
               .build();
    }

    private final String name;
    private final String email;

}
