package com.cae.loggers;

import com.cae.loggers.native_io_extraction_mode.NativeExtractionMode;

public class IOLoggingHandler {

    private IOLoggingHandler(){}

    public static String generateTextForLoggingInput(Object input, String prefix){
        return " [" +
                prefix +
                " INPUT]: " +
                (AutologProvider.SINGLETON.getIOLoggingMode().equals(IOLoggingMode.CAE_NATIVE) ? IOLoggingHandler.handleNativeExtractionOf(input) : input.toString()) +
                ";";
    }

    public static String generateTextForLoggingOutput(Object output, String prefix){
        return " [" +
                prefix +
                " OUTPUT]: " +
                (AutologProvider.SINGLETON.getIOLoggingMode().equals(IOLoggingMode.CAE_NATIVE) ? IOLoggingHandler.handleNativeExtractionOf(output) : output.toString()) +
                ";";
    }

    private static String handleNativeExtractionOf(Object object) {
        return NativeExtractionMode.executeOn(object);
    }

}
