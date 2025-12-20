package com.cae.framework.ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.exceptions.PortExecutionException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import utils.normal_ports.SomeNormalConsumerPort;
import utils.problematic_ports.SomeProblematicConsumerPort;

public class ConsumerPortTest {

    private final SomeNormalConsumerPort unit = new SomeNormalConsumerPort();
    private final SomeProblematicConsumerPort problematicUnit = new SomeProblematicConsumerPort();
    private final String input = "input";
    private ExecutionContext executionContext;

    @BeforeEach
    void setup(){
        this.executionContext = ExecutionContext.ofNew();
        this.executionContext.setSubjectAndStartTracking("SupplierPortTests", true);
    }

    @Test
    @DisplayName("Should add itself as a step when executed successfully")
    void shouldAddItselfAsStepWhenExecuted(){
        Assertions.assertTrue(this.executionContext.getStepInsights().isEmpty());
        this.unit.executePortOn(this.input, this.executionContext);
        Assertions.assertFalse(this.executionContext.getStepInsights().isEmpty());
        Assertions.assertEquals(1, this.executionContext.getStepInsights().size());
        Assertions.assertEquals(this.unit.getName(), this.executionContext.getStepInsights().get(0).getSubject());
    }

    @Test
    @DisplayName("Should add input info to the step insights when executed successfully")
    void shouldAddIOInfoToTheStepInsightsWhenExecutedSuccessfully(){
        this.unit.executePortOn(this.input, this.executionContext);
        Assertions.assertEquals(this.input, this.executionContext.getStepInsights().get(0).getInput());
    }

    @Test
    @DisplayName("Should complete its own step when finishes successfully")
    void shouldCompleteItsOwnStepWhenFinishesSuccessfully(){
        this.unit.executePortOn(this.input, this.executionContext);
        Assertions.assertTrue(this.executionContext.getStepInsights().get(0).wasSuccessful());
    }

    @Test
    @DisplayName("Should complete its own step when finishes unsuccessfully")
    void shouldCompleteItsOwnStepWhenFinishesUnsuccessfully(){
        try{
            this.problematicUnit.executePortOn(this.input, this.executionContext);
        } catch (PortExecutionException e){
            Assertions.assertFalse(this.executionContext.getStepInsights().get(0).wasSuccessful());
            Assertions.assertTrue(e.getOriginalException().isPresent());
            Assertions.assertEquals(e.getOriginalException().get(), this.executionContext.getStepInsights().get(0).getException());
        }
    }

    @Test
    @DisplayName("Should add itself as a step when executed unsuccessfully")
    void shouldAddItselfAsStepWhenExecutedUnsuccessfully(){
        Assertions.assertTrue(this.executionContext.getStepInsights().isEmpty());
        try {
            this.problematicUnit.executePortOn(this.input, this.executionContext);
        } catch (PortExecutionException e){
            Assertions.assertFalse(this.executionContext.getStepInsights().isEmpty());
            Assertions.assertEquals(1, this.executionContext.getStepInsights().size());
            Assertions.assertEquals(this.problematicUnit.getName(), this.executionContext.getStepInsights().get(0).getSubject());
        }
    }

    @Test
    @DisplayName("Should be able to transform unexpected exceptions into PortExecutionException instances")
    void shouldBeAbleToTransformUnexpectedExceptionsIntoPortExecutionExceptionInstances(){
        try{
            this.problematicUnit.executePortOn(this.input, this.executionContext);
        } catch (Exception e){
            Assertions.assertInstanceOf(PortExecutionException.class, e);
            var portEx = (PortExecutionException) e;
            Assertions.assertTrue(portEx.getOriginalException().isPresent());
        }
    }

}
