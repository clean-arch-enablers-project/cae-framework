package utils;

import com.cae.loggers.Logger;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LoggerAdapterForTesting implements Logger {

    public static final LoggerAdapterForTesting SINGLETON = new LoggerAdapterForTesting();

    @Override
    public void logInfo(String info) {
        System.out.println(info);
    }

    @Override
    public void logError(String error) {
        System.out.println(error);
    }

    @Override
    public void logDebug(String info) {
        System.out.println(info);
    }
}
