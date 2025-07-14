package com.cae.autofeatures;

import com.cae.autofeatures.autoauth.RoleRetriever;
import com.cae.autofeatures.autoauth.RoleRetrieverRegistry;
import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.IOAutologMode;
import com.cae.autofeatures.autolog.Logger;
import com.cae.autofeatures.autometrics.AutometricsProvider;
import com.cae.autofeatures.autometrics.AutometricsSubscriber;
import com.cae.autofeatures.autonotify.AutonotifyProvider;
import com.cae.autofeatures.autonotify.AutonotifySubscriber;

public class CaeSetup {

    public static final CaeSetup SINGLETON = new CaeSetup();

    public AutologSetup autolog(){
        return new AutologSetup();
    }

    public AutometricsSetup autometrics(){
        return new AutometricsSetup();
    }

    public AutonotifySetup autonotify(){
        return new AutonotifySetup();
    }

    public AutoauthSetup autoauth(){
        return new AutoauthSetup();
    }

    public void reset(){
        AutologProvider.SINGLETON.reset();
        AutometricsProvider.SINGLETON.reset();
        AutonotifyProvider.SINGLETON.reset();
        RoleRetrieverRegistry.SINGLETON.reset();
    }

    public static class AutologSetup{

        public AutologSetup setProvidedInstance(Logger logger){
            AutologProvider.SINGLETON.setProvidedInstance(logger);
            return this;
        }

        public AutologSetup setAsync(boolean async){
            AutologProvider.SINGLETON.setAsync(async);
            return this;
        }

        public AutologSetup setUseCasesLoggingIO(boolean useCasesLoggingIO){
            AutologProvider.SINGLETON.setUseCasesLoggingIO(useCasesLoggingIO);
            return this;
        }

        public AutologSetup setPortsLoggingIO(boolean portsLoggingIO){
            AutologProvider.SINGLETON.setPortsLoggingIO(portsLoggingIO);
            return this;
        }

        public AutologSetup setIOAutologMode(IOAutologMode ioAutologMode){
            AutologProvider.SINGLETON.setIOAutologMode(ioAutologMode);
            return this;
        }

        public AutologSetup setStructuredFormat(boolean structuredFormat){
            AutologProvider.SINGLETON.setStructuredFormat(structuredFormat);
            return this;
        }

        public AutologSetup setLoggingStackTrace(boolean loggingStackTrace){
            AutologProvider.SINGLETON.setLoggingStackTrace(loggingStackTrace);
            return this;
        }

        public AutologSetup setNumberOfLinesFromStackTrace(int numberOfLines){
            AutologProvider.SINGLETON.setNumberOfLinesFromStackTrace(numberOfLines);
            return this;
        }

        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    public static class AutometricsSetup{

        public AutometricsSetup setAsync(boolean async){
            AutometricsProvider.SINGLETON.setAsync(async);
            return this;
        }

        public AutometricsSetup subscribe(AutometricsSubscriber subscriber){
            AutometricsProvider.SINGLETON.subscribe(subscriber);
            return this;
        }

        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    public static class AutonotifySetup{

        public AutonotifySetup setAsync(boolean async){
            AutonotifyProvider.SINGLETON.setAsync(async);
            return this;
        }

        public AutonotifySetup subscribe(AutonotifySubscriber subscriber){
            AutonotifyProvider.SINGLETON.subscribe(subscriber);
            return this;
        }

        public AutonotifySetup considerInputMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerInputMappedExceptions();
            return this;
        }

        public AutonotifySetup considerNotFoundMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotFoundMappedExceptions();
            return this;
        }

        public AutonotifySetup considerNotAuthenticatedMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotAuthenticatedMappedExceptions();
            return this;
        }

        public AutonotifySetup considerNotAuthorizedMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerNotAuthorizedMappedExceptions();
            return this;
        }

        public AutonotifySetup considerInternalMappedExceptions(){
            AutonotifyProvider.SINGLETON.considerInternalMappedExceptions();
            return this;
        }

        public AutonotifySetup considerNoRetriesLeftExceptions(){
            AutonotifyProvider.SINGLETON.considerNoRetriesLeftExceptions();
            return this;
        }

        public AutonotifySetup considerMissingEnvVarExceptions(){
            AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
            return this;
        }

        public AutonotifySetup considerUnexpectedExceptions(){
            AutonotifyProvider.SINGLETON.considerUnexpectedExceptions();
            return this;
        }

        public <T extends Exception> AutonotifySetup considerSpecifically(Class<T> exceptionType){
            AutonotifyProvider.SINGLETON.considerSpecifically(exceptionType);
            return this;
        }

        public AutonotifySetup considerLatency(Integer thresholdInMillis){
            AutonotifyProvider.SINGLETON.considerLatency(thresholdInMillis);
            return this;
        }

        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

    public static class AutoauthSetup{

        public AutoauthSetup setDefaultRoleRetriever(RoleRetriever roleRetriever){
            RoleRetrieverRegistry.SINGLETON.setDefaultRoleRetriever(roleRetriever);
            return this;
        }

        public AutoauthSetup setRetrieverByUseCaseId(RoleRetriever roleRetriever, String useCaseId){
            RoleRetrieverRegistry.SINGLETON.putRetrieverByUseCaseId(roleRetriever, useCaseId);
            return this;
        }


        public CaeSetup done(){
            return CaeSetup.SINGLETON;
        }

    }

}
