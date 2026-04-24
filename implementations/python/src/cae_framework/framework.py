from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Generic, TypeVar, get_type_hints
from cae_context import ExecutionContext
from cae_trier import trier_of
from cae_mapped_exceptions import (
    MappedException,
    InternalMappedException,
    InputMappedException
)
from collections.abc import Iterable

INPUT = TypeVar("INPUT")
OUTPUT = TypeVar("OUTPUT")


class UseCaseMetadata:
    def __init__(self, use_case: UseCase):
        self.name: str = type(use_case).__name__


class UseCase(ABC):

    def __init__(self):
        super().__init__()
        self.metadata: UseCaseMetadata = UseCaseMetadata(self)

    def _handle_unexpected_problems(
            self,
            problem: Exception
    ) -> MappedException:
        details = "You might want to handle this scenario, " +\
            "transforming it into a MappedException"
        return UseCaseExecutionMappedException(
            "Some unexpected problem happened",
            details,
            problem
        )


class FunctionUseCase(UseCase, Generic[INPUT, OUTPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, input: INPUT, context: ExecutionContext) -> OUTPUT:
        return trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(input, context)

    def __run(self, input: INPUT, context: ExecutionContext) -> OUTPUT:
        context.set_subject_and_start_tracking(self.metadata.name, True)
        context.input = input
        try:
            PreExecutionAutofeatures.run(input, context)
            output = self.__get_output_for(input, context)
            context.complete()
            context.output = output
            PostExecutionAutofeatures.run(context)
            return output
        except Exception as any_exception:
            context.complete_with_ex(any_exception)
            PostExecutionAutofeatures.run(context)
            raise any_exception

    def __get_output_for(
            self,
            input: INPUT,
            context: ExecutionContext
    ) -> OUTPUT:
        return self._apply_internal_logic(input, context)

    @abstractmethod
    def _apply_internal_logic(
        self,
        input: INPUT,
        context: ExecutionContext
    ) -> OUTPUT:
        pass


class ConsumerUseCase(UseCase, Generic[INPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, input: INPUT, context: ExecutionContext) -> None:
        trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(input, context)

    def __run(self, input: INPUT, context: ExecutionContext) -> None:
        context.set_subject_and_start_tracking(self.metadata.name, True)
        context.input = input
        try:
            PreExecutionAutofeatures.run(input, context)
            self._apply_internal_logic(input, context)
            context.complete()
            PostExecutionAutofeatures.run(context)
        except Exception as any_exception:
            context.complete_with_ex(any_exception)
            PostExecutionAutofeatures.run(context)
            raise any_exception

    @abstractmethod
    def _apply_internal_logic(
        self,
        input: INPUT,
        context: ExecutionContext
    ) -> None:
        pass


class SupplierUseCase(UseCase, Generic[OUTPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, context: ExecutionContext) -> OUTPUT:
        return trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(context)

    def __run(self, context: ExecutionContext) -> OUTPUT:
        try:
            context.set_subject_and_start_tracking(self.metadata.name, True)
            PreExecutionAutofeatures.run_with_no_input(context)
            output = self.__get_output_for(context)
            context.complete()
            context.output = output
            PostExecutionAutofeatures.run(context)
            return output
        except Exception as any_exception:
            context.complete_with_ex(any_exception)
            PostExecutionAutofeatures.run(context)
            raise any_exception

    def __get_output_for(
            self,
            context: ExecutionContext
    ) -> OUTPUT:
        return self._apply_internal_logic(context)

    @abstractmethod
    def _apply_internal_logic(
        self,
        context: ExecutionContext
    ) -> OUTPUT:
        pass


class RunnableUseCase(UseCase):

    def __init__(self):
        super().__init__()

    def execute(self, context: ExecutionContext) -> None:
        trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(context)

    def __run(self, context: ExecutionContext) -> None:
        try:
            context.set_subject_and_start_tracking(self.metadata.name, True)
            PreExecutionAutofeatures.run_with_no_input(context)
            self._apply_internal_logic(context)
            context.complete()
            PostExecutionAutofeatures.run(context)
        except Exception as any_exception:
            context.complete_with_ex(any_exception)
            PostExecutionAutofeatures.run(context)
            raise any_exception

    @abstractmethod
    def _apply_internal_logic(
        self,
        context: ExecutionContext
    ) -> None:
        pass


class UseCaseExecutionMappedException(InternalMappedException):

    def __init__(
            self,
            brief_public_message: str | None = None,
            details: str | None = None,
            original: Exception | None = None
    ):
        super().__init__(brief_public_message, details, original)


class Port(ABC):

    def __init__(self):
        super().__init__()
        self.port_name = type(self).__name__

    def _handle_unexpected_problems(
            self,
            problem: Exception
    ) -> MappedException:
        details = "You might want to handle this scenario, " +\
            "transforming it into a MappedException"
        return PortExecutionMappedException(
            "Some unexpected problem happened",
            details,
            problem
        )


class FunctionPort(Port, Generic[INPUT, OUTPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, input: INPUT, context: ExecutionContext) -> OUTPUT:
        return trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(input, context)

    def __run(self, input: INPUT, context: ExecutionContext) -> OUTPUT:
        step = context.add_step_insights_of(self.port_name)
        step.input = input
        try:
            output = self.__get_output_for(input, context)
            step.complete()
            step.output = output
            return output
        except Exception as any_exception:
            step.complete_with_ex(any_exception)
            raise any_exception

    def __get_output_for(
            self,
            input: INPUT,
            context: ExecutionContext
    ) -> OUTPUT:
        return self._apply_internal_logic(input, context)

    @abstractmethod
    def _apply_internal_logic(
        self,
        input: INPUT,
        context: ExecutionContext
    ) -> OUTPUT:
        pass


class ConsumerPort(Port, Generic[INPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, input: INPUT, context: ExecutionContext) -> None:
        trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(input, context)

    def __run(self, input: INPUT, context: ExecutionContext) -> None:
        step = context.add_step_insights_of(self.port_name)
        step.input = input
        try:
            self._apply_internal_logic(input, context)
            step.complete()
        except Exception as any_exception:
            step.complete_with_ex(any_exception)
            raise any_exception

    @abstractmethod
    def _apply_internal_logic(
        self,
        input: INPUT,
        context: ExecutionContext
    ) -> None:
        pass


class SupplierPort(Port, Generic[OUTPUT]):

    def __init__(self):
        super().__init__()

    def execute(self, context: ExecutionContext) -> OUTPUT:
        return trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(context)

    def __run(self, context: ExecutionContext) -> OUTPUT:
        step = context.add_step_insights_of(self.port_name)
        try:
            output = self.__get_output_for(context)
            step.complete()
            step.output = output
            return output
        except Exception as any_exception:
            step.complete_with_ex(any_exception)
            raise any_exception

    def __get_output_for(
            self,
            context: ExecutionContext
    ) -> OUTPUT:
        return self._apply_internal_logic(context)

    @abstractmethod
    def _apply_internal_logic(
        self,
        context: ExecutionContext
    ) -> OUTPUT:
        pass


class RunnablePort(Port):

    def __init__(self):
        super().__init__()

    def execute(self, context: ExecutionContext) -> None:
        trier_of(self.__run).\
            on_unexpected_exceptions_do(self._handle_unexpected_problems).\
            execute(context)

    def __run(self, context: ExecutionContext) -> None:
        step = context.add_step_insights_of(self.port_name)
        try:
            self._apply_internal_logic(context)
            step.complete()
        except Exception as any_exception:
            step.complete_with_ex(any_exception)
            raise any_exception

    @abstractmethod
    def _apply_internal_logic(
        self,
        context: ExecutionContext
    ) -> None:
        pass


class PortExecutionMappedException(InternalMappedException):

    def __init__(
            self,
            brief_public_message: str | None = None,
            details: str | None = None,
            original: Exception | None = None
    ):
        super().__init__(brief_public_message, details, original)


class PreExecutionAutofeatures:

    @classmethod
    def run(cls, input: object, context: ExecutionContext) -> None:
        Autoverify.run(input, context)

    @classmethod
    def run_with_no_input(cls, context: ExecutionContext) -> None:
        pass


class PostExecutionAutofeatures:

    @classmethod
    def run(cls, context: ExecutionContext) -> None:
        Autolog.run(context)


class Autoverify:

    @classmethod
    def run(cls, input: object, context: ExecutionContext) -> None:
        step = context.add_step_insights_of("Autoverify")
        try:
            hints = get_type_hints(type(input), include_extras=True)
            for hint in hints:
                actual_value = getattr(input, hint)
                metadata = getattr(hints[hint], "__metadata__", ())
                validation_subjects = cls.filter_validation_subjects(metadata)
                for validation_subject in validation_subjects:
                    validation_subject.validate(actual_value, hint)
            step.complete()
        except MappedException as mapped_ex:
            step.complete_with_ex(mapped_ex)
            raise mapped_ex
        except Exception as unexpected_ex:
            step.complete_with_ex(unexpected_ex)
            raise InternalMappedException.with_brief_public_message(
                "Something went unexpectedly wrong",
                unexpected_ex
            )

    @classmethod
    def filter_validation_subjects(
            cls,
            metadata: Iterable[object]
    ) -> list[ValidationSubject]:
        return [
            x for x in metadata
            if isinstance(x, ValidationSubject)
        ]


class ValidationSubject(ABC):

    @abstractmethod
    def validate(self, value: object, field_name: str) -> None:
        pass


class NotBlank(ValidationSubject):

    def validate(self, value: object, field_name: str) -> None:
        if not isinstance(value, str):
            raise AutoverifyTypeErrorMappedException(
                f"{field_name} must be a string to use @NotBlank"
            )
        if value.strip() == "":
            raise InvalidInputFieldMappedException(
                f"{field_name} can't be blank"
            )


class NotNone(ValidationSubject):

    def validate(self, value: object, field_name: str) -> None:
        if value is None:
            raise InvalidInputFieldMappedException(
                f"{field_name} can't be None"
            )


class AutoverifyTypeErrorMappedException(InternalMappedException):

    def __init__(
            self,
            brief_public_message: str | None = None,
            details: str | None = None,
            original: Exception | None = None
    ):
        super().__init__(brief_public_message, details, original)


class InvalidInputFieldMappedException(InputMappedException):

    def __init__(
            self,
            brief_public_message: str | None = None,
            details: str | None = None,
            original: Exception | None = None
    ):
        super().__init__(brief_public_message, details, original)


class AutologLogger(ABC):

    @abstractmethod
    def log_info(self, info: str) -> None:
        pass

    @abstractmethod
    def log_error(self, error: str) -> None:
        pass


class Autolog:

    @classmethod
    def run(cls, context: ExecutionContext) -> None:
        pipe = " | "
        header = (
            f"Subject '{context.subject}' execution with correlation ID of "
            f"{context.correlation_id} "
        )
        if context.was_successful():
            status = (
                f"finished successfully and took about "
                f"{context.get_latency()}ms"
            )
        else:
            status = (
                f"threw an exception and took about "
                f"{context.get_latency()}ms: {context.exception}"
            )
        step_texts = [s.format_as_text() for s in context.step_insights]
        steps = str(step_texts)
        input_prefix = "Input: "
        if context.input is not None:
            if hasattr(context.input, "__dict__"):
                input_data = input_prefix + str(vars(context.input))
            else:
                input_data = input_prefix + str(context.input)
        else:
            input_data = ""
        output_prefix = "Output: "
        if context.output is not None:
            if hasattr(context.output, "__dict__"):
                output_data = output_prefix + str(vars(context.output))
            else:
                output_data = output_prefix + str(context.output)
        else:
            output_data = ""
        message = (
            header
            + status
            + pipe
            + steps
            + pipe
            + input_data
            + pipe
            + output_data
        )
        logger = CaeSetup.get_autolog_logger()
        if context.was_successful():
            logger.log_info(message)
        else:
            logger.log_error(message)


class CaeSetup:

    _autolog_logger: AutologLogger | None = None

    @classmethod
    def set_autolog_logger(cls, logger: AutologLogger) -> None:
        cls._autolog_logger = logger

    @classmethod
    def get_autolog_logger(cls) -> AutologLogger:
        if cls._autolog_logger is None:
            raise RuntimeError("AutologLogger has not been configured")
        return cls._autolog_logger
