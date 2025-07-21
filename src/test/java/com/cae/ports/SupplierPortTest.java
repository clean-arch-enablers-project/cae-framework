package com.cae.ports;

import com.cae.ports.exceptions.PortExecutionException;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.normal_ports.SomeNormalSupplierPort;
import utils.problematic_ports.SomeProblematicSupplierPort;

@ExtendWith(MockitoExtension.class)
class SupplierPortTest {

    private final SomeNormalSupplierPort unit = new SomeNormalSupplierPort();
    private final SomeProblematicSupplierPort problematicUnit = new SomeProblematicSupplierPort();
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
        this.unit.executePort(this.executionContext);
        Assertions.assertFalse(this.executionContext.getStepInsights().isEmpty());
        Assertions.assertEquals(1, this.executionContext.getStepInsights().size());
        Assertions.assertEquals(this.unit.getName(), this.executionContext.getStepInsights().get(0).getSubject());
    }

    @Test
    @DisplayName("Should set its output at the step")
    void shouldSetItsOutputAtTheStep(){
        var output = this.unit.executePort(this.executionContext);
        Assertions.assertEquals(output, this.executionContext.getStepInsights().get(0).getOutput());
    }

    @Test
    @DisplayName("Should complete its own step when finishes successfully")
    void shouldCompleteItsOwnStepWhenFinishesSuccessfully(){
        this.unit.executePort(this.executionContext);
        Assertions.assertTrue(this.executionContext.getStepInsights().get(0).wasSuccessful());
    }

    @Test
    @DisplayName("Should complete its own step when finishes unsuccessfully")
    void shouldCompleteItsOwnStepWhenFinishesUnsuccessfully(){
        try{
            this.problematicUnit.executePort(this.executionContext);
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
            this.problematicUnit.executePort(this.executionContext);
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
            this.problematicUnit.executePort(this.executionContext);
        } catch (Exception e){
            Assertions.assertInstanceOf(PortExecutionException.class, e);
            var portEx = (PortExecutionException) e;
            Assertions.assertTrue(portEx.getOriginalException().isPresent());
        }
    }

}
