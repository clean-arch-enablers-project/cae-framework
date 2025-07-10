package com.cae.autofeatures.autolog.formats;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class IO {

    private final Object input;
    private final Object output;

}
