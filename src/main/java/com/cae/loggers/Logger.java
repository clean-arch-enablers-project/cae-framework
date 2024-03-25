package com.cae.loggers;

/**
 * Architectural border. Implement this interface with your preferred
 * log configurations in order to use it inside your core logic layer
 * without making the core layer coupled to your current logging
 * preferences.
 */
public interface Logger {

    /**
     * Method for logging normal info
     * @param info the text to log
     */
    void logInfo(String info);

    /**
     * Method for logging error info
     * @param error the text to log
     */
    void logError(String error);

    /**
     * Method for logging debug info
     * @param info the text to log
     */
    void logDebug(String info);

}
