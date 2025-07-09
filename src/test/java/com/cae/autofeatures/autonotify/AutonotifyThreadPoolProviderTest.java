package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.AutofeatureThreadPoolProvider;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AutonotifyThreadPoolProviderTest {

    @Test
    @DisplayName("Should be instance of AutofeatureThreadPoolProvider")
    void shouldBeInstanceOfAutofeatureThreadPoolProvider(){
        Assertions.assertInstanceOf(AutofeatureThreadPoolProvider.class, AutonotifyThreadPoolProvider.SINGLETON);
    }

}
