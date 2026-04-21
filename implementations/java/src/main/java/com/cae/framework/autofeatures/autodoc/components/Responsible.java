package com.cae.framework.autofeatures.autodoc.components;

import lombok.*;

@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Responsible {

    public static Responsible of(String name, String email){
       return Responsible.builder()
               .name(name)
               .email(email)
               .build();
    }

    private String name;
    private String email;

}
