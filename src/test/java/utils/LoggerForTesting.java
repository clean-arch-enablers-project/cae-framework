package utils;

import com.cae.loggers.Logger;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.Optional;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LoggerForTesting implements Logger {

    private static LoggerForTesting singletonInstance;

    public static Logger getSingletonInstance(){
        if (Optional.ofNullable(singletonInstance).isEmpty())
            singletonInstance = new LoggerForTesting();
        return singletonInstance;
    }

    @Override
    public void logInfo(String info) {
        log.info(info);
    }

    @Override
    public void logError(String error) {
        log.error(error);
    }

    @Override
    public void logDebug(String info) {
        log.debug(info);
    }
}
