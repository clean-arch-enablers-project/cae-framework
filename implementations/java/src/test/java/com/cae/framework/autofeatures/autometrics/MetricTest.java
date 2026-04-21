package com.cae.framework.autofeatures.autometrics;

import com.cae.context.ExecutionContext;
import com.cae.mapped_exceptions.specifics.NotFoundMappedException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MetricTest {

    @Test
    @DisplayName("Should create metrics as expected")
    void shouldCreateMetricsAsExpected() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("some-use-case", true);
        var stepOne = execContext.addStepInsightsOf("port-one");
        Thread.sleep(40);
        stepOne.complete();
        var stepTwo = execContext.addStepInsightsOf("port-two");
        Thread.sleep(30);
        stepTwo.complete();
        var stepThree = execContext.addStepInsightsOf("port-three");
        Thread.sleep(150);
        stepThree.complete();
        execContext.complete();
        var metrics = Metric.createNewOnesBasedOn(execContext);
        Assertions.assertEquals(4, metrics.size());
        var hasMetricForUseCase = metrics.stream()
                .anyMatch(metric -> metric.getSubjectName().equals(execContext.getSubject()));
        var hasMetricForStepOne = metrics.stream()
                .anyMatch(metric -> metric.getSubjectName().equals(stepOne.getSubject()));
        var hasMetricForStepTwo = metrics.stream()
                .anyMatch(metric -> metric.getSubjectName().equals(stepTwo.getSubject()));
        var hasMetricForStepThree = metrics.stream()
                .anyMatch(metric -> metric.getSubjectName().equals(stepThree.getSubject()));
        var allMetricsHaveTheSameCorrelationId = metrics.stream()
                .allMatch(metric -> metric.getCorrelationId().equals(execContext.getCorrelationId()));
        var allMetricsHaveLatency = metrics.stream()
                .allMatch(metric -> metric.getLatency() != null);
        var allMetricsHaveStartTime = metrics.stream()
                .allMatch(metric -> metric.getStartingTime() != null);
        var allMetricsHaveEndTime = metrics.stream()
                .allMatch(metric -> metric.getEndingTime() != null);
        var allMetricsHaveSuccessStatus = metrics.stream()
                .allMatch(metric -> metric.getSuccess() != null);
        var noMetricHasExceptionBecauseEveryStepWasSuccessful = metrics.stream()
                .allMatch(metric -> metric.getException() == null);
        var allPortMetricsAreMarkedAsOutbound = metrics.stream()
                .filter(metric -> metric.getSubjectName().contains("port-"))
                .noneMatch(Metric::getInbound);
        var useCaseMetricIsMarkedAsInbound = metrics.stream()
                .filter(metric -> metric.getSubjectName().contains("some-use-case"))
                .allMatch(Metric::getInbound);
        Assertions.assertTrue(hasMetricForUseCase);
        Assertions.assertTrue(hasMetricForStepOne);
        Assertions.assertTrue(hasMetricForStepTwo);
        Assertions.assertTrue(hasMetricForStepThree);
        Assertions.assertTrue(allMetricsHaveTheSameCorrelationId);
        Assertions.assertTrue(allMetricsHaveLatency);
        Assertions.assertTrue(allMetricsHaveStartTime);
        Assertions.assertTrue(allMetricsHaveEndTime);
        Assertions.assertTrue(allMetricsHaveSuccessStatus);
        Assertions.assertTrue(noMetricHasExceptionBecauseEveryStepWasSuccessful);
        Assertions.assertTrue(allPortMetricsAreMarkedAsOutbound);
        Assertions.assertTrue(useCaseMetricIsMarkedAsInbound);
    }

    @Test
    @DisplayName("When execution fails must include exception into the metric")
    void whenExecutionFailsMustIncludeExceptionIntoTheMetric() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("some-other-use-case", true);
        Thread.sleep(30);
        execContext.complete(new NotFoundMappedException("ops!"));
        var metrics = Metric.createNewOnesBasedOn(execContext);
        Assertions.assertEquals(1, metrics.size());
        Assertions.assertEquals(execContext.getException(), metrics.get(0).getException());
        Assertions.assertFalse(metrics.get(0).getSuccess());
    }

}
