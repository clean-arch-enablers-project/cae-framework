package com.cae.loggers;

public class IOLoggingHandler {

    private IOLoggingHandler(){}

    public static String generateTextForLoggingInput(Object input, String prefix){
        return " [" +
                prefix +
                " INPUT]: \"" +
                (LoggerProvider.SINGLETON.getIOLoggingMode().equals(LoggerProvider.IOLoggingMode.CAE_NATIVE) ? IOLoggingHandler.handleNativeExtractionOf(input) : input.toString()) +
                "\";";
    }

    public static String generateTextForLoggingOutput(Object output, String prefix){
        return " [" +
                prefix +
                " OUTPUT]: \"" +
                (LoggerProvider.SINGLETON.getIOLoggingMode().equals(LoggerProvider.IOLoggingMode.CAE_NATIVE) ? IOLoggingHandler.handleNativeExtractionOf(output) : output.toString()) +
                "\";";
    }

    private static String handleNativeExtractionOf(Object object) {
        return "";
    }

}
