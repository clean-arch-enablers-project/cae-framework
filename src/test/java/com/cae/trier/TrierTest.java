package com.cae.trier;

import com.cae.mapped_exceptions.MappedException;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@Slf4j
class TrierTest {

    private final RuntimeException unexpectedInternalException = new RuntimeException("oh no!");
    private final MappedException expectedInternalException = new MappedException("right... I was kinda counting on this happening");
    private final MappedException exceptionToHandleTheUnexpected = new MappedException("MappedException to the rescue!");

    @Test
    void shouldRunFunctionActionWithoutProblems(){
        var result = Trier.of(this::functionAction, "some input")
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                .andExecuteTheAction();
        Assertions.assertNotNull(result);
        Assertions.assertEquals("Function action worked! You used 'some input' as input", result);
    }

    @Test
    void shouldRunConsumerActionWithoutProblems(){
        var trierBuilder = Trier.of(this::consumerAction, "some input")
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected);
        Assertions.assertDoesNotThrow(trierBuilder::andExecuteTheAction);
    }

    @Test
    void shouldRunSupplierActionWithoutProblems(){
        var result = Trier.of(this::supplierAction)
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                .andExecuteTheAction();
        Assertions.assertNotNull(result);
        Assertions.assertEquals("Supplier action worked!", result);
    }

    @Test
    void shouldRunRunnableActionWithoutProblems(){
        var trierBuilder = Trier.of(this::runnableAction)
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected);
        Assertions.assertDoesNotThrow(trierBuilder::andExecuteTheAction);
    }

    @Test
    void shouldHandleUnexpectedProblematicFunctionAction(){
        try{
            Trier.of(this::unexpectedProblematicFunctionAction, "some input")
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertNotEquals(this.unexpectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleUnexpectedProblematicConsumerAction(){
        var trierBuilder = Trier.of(this::unexpectedProblematicConsumerAction, "some input")
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected);
        try{
            trierBuilder.andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertNotEquals(this.unexpectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleUnexpectedProblematicSupplierAction(){
        try {
            Trier.of(this::unexpectedProblematicSupplierAction)
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertNotEquals(this.unexpectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleUnexpectedProblematicRunnableAction(){
        var trierBuilder = Trier.of(this::unexpectedProblematicRunnableAction)
                .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected);
        try{
            trierBuilder.andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertNotEquals(this.unexpectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleExpectedProblematicFunctionAction(){
        try{
            Trier.of(this::expectedProblematicFunctionAction, "some input")
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertNotEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertEquals(this.expectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleExpectedProblematicConsumerAction(){
        try{
            Trier.of(this::expectedProblematicConsumerAction, "some input")
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertNotEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertEquals(this.expectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleExpectedProblematicSupplierAction(){
        try{
            Trier.of(this::expectedProblematicSupplierAction)
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertNotEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertEquals(this.expectedInternalException, caughtException);
        }
    }

    @Test
    void shouldHandleExpectedProblematicRunnableAction(){
        try{
            Trier.of(this::expectedProblematicRunnableAction)
                    .prepareForUnexpectedExceptionsUsing(unexpectedException -> this.exceptionToHandleTheUnexpected)
                    .andExecuteTheAction();
        } catch (Exception caughtException){
            Assertions.assertTrue(caughtException instanceof MappedException);
            Assertions.assertNotEquals(caughtException, this.exceptionToHandleTheUnexpected);
            Assertions.assertEquals(this.expectedInternalException, caughtException);
        }
    }

    String functionAction(String input){
        return "Function action worked! You used '" + input + "' as input";
    }

    void consumerAction(String input){
        log.info("Consumer action worked! You used '" + input + "' as input");
    }

    String supplierAction(){
        return "Supplier action worked!";
    }

    void runnableAction(){
        log.info("Runnable action worked!");
    }

    String unexpectedProblematicFunctionAction(String input){
        throw this.unexpectedInternalException;
    }

    void unexpectedProblematicConsumerAction(String input){
        throw this.unexpectedInternalException;
    }

    String unexpectedProblematicSupplierAction(){
        throw this.unexpectedInternalException;
    }

    void unexpectedProblematicRunnableAction(){
        throw this.unexpectedInternalException;
    }

    String expectedProblematicFunctionAction(String input){
        throw this.expectedInternalException;
    }

    void expectedProblematicConsumerAction(String input){
        throw this.expectedInternalException;
    }

    String expectedProblematicSupplierAction(){
        throw this.expectedInternalException;
    }

    void expectedProblematicRunnableAction(){
        throw this.expectedInternalException;
    }

}
