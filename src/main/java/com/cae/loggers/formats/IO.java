package com.cae.loggers.formats;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class IO {

    private Object input;
    private Object output;

}
