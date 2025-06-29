package com.cae.autofeatures.autolog;

import com.cae.autofeatures.autolog.native_io_extraction_mode.NativeExtractionMode;

public class IOAutologHandler {

    private IOAutologHandler(){}

    public static String generateTextForLoggingInput(Object input, String prefix){
        return " [" +
                prefix +
                " INPUT]: " +
                (AutologProvider.SINGLETON.getIOLoggingMode().equals(IOAutologMode.CAE_NATIVE) ? IOAutologHandler.handleNativeExtractionOf(input) : input.toString()) +
                ";";
    }

    public static String generateTextForLoggingOutput(Object output, String prefix){
        return " [" +
                prefix +
                " OUTPUT]: " +
                (AutologProvider.SINGLETON.getIOLoggingMode().equals(IOAutologMode.CAE_NATIVE) ? IOAutologHandler.handleNativeExtractionOf(output) : output.toString()) +
                ";";
    }

    private static String handleNativeExtractionOf(Object object) {
        return NativeExtractionMode.executeOn(object);
    }

}
