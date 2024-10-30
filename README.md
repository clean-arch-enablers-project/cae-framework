# ✔️ cae-framework
☕ Java & Kotlin edition

<br>

Welcome to the open source cae-framework repository! This component of the SDK is designed to enable the [Angularization](https://github.com/clean-arch-enablers-project#-the-angularization-concept) effect of client applications by following Clean Architecture principles.

### ▶️ The artifact:

```xml
<dependency>
  <groupId>com.clean-arch-enablers</groupId>
  <artifactId>cae-framework</artifactId>
  <version>${version}</version>
</dependency>
```
###### All available versions can be found here: [cae-framework on Maven](https://central.sonatype.com/artifact/com.clean-arch-enablers/cae-framework) (_since it is in a snapshot state, it is recommended to always use the latest version._) 

<br>

State Symbol Key:

- ``✅`` — _Under release state_
- ``✔️`` — _Under snapshot state_
- ``⏳`` — _Under full development state_

<br>

## 📚 Key Concepts
### ⚙️ Use Cases
The core of the CAE Framework revolves around use cases. Each use case is a distinct, self-contained unit of functionality, designed to be easily maintained, extended, and tested.

##### 🎨 Types of Use Cases
- ``✔️`` ``FunctionUseCase``: Receives input and returns output.
- ``✔️`` ``ConsumerUseCase``: Receives input but does not return output.
- ``✔️`` ``SupplierUseCase``: Returns output without any input.
- ``✔️`` ``RunnableUseCase``: Neither receives input nor returns output.

#### ▶️ Use Case Execution
A use case, when executed, can have some behaviors:

- ``✔️`` Autolog
- ``✔️`` Auto input validation
- ``⏳`` Autocache
- ``⏳`` Auto notification
- ``✔️`` Scope based authorization
- ``⏳`` Role based authorization

##### 📄 Autolog
Whenever an instance of use case gets executed, an automatic log will be generated. It can be in two modes:

- structured
- natural language

The structured mode is a JSON payload with the log data, while the natural language mode is basically a simple text. However, both can display the same data; they just differ in their presentation styles.

The contained info is:

- The use case: the name of the use case which is being executed (the object's class name)
- Execution correlation Id: an unique identifier per execution (can be parameterized or randomly generated)
- Whether or not successful: if the flow didn't throw any exceptions
- Exception thrown (in case of unsuccessful executions): what went wrong
- Latency
- Port insights: insights of what's going on during the execution of each of the use case's ports
- IO data: what the use case execution received as input and what returned as output

An example for the structured format:

```json
{
    "useCaseExecution": {
        "useCase": "auth_root_account_implementation",
        "correlationId": "c6268eb5-2f2b-48e3-8138-f5636af216b4",
        "successful": "true",
        "exception": null,
        "latency": "31",
        "portInsights": [
            "FindRootAccountByLoginIdPortAdapter's insights: no exception has been thrown",
            "FindRootAccountSecretPortAdapter's insights: no exception has been thrown",
            "SessionTokenGenerationPortAdapter's insights: no exception has been thrown"
        ],
        "io": {
            "input": {
                "loginId": "caertsc@capitolio.com",
                "pass": "**********"
            },
            "output": {
                "expiration": "3600",
                "name": "Capitólio",
                "id": "4",
                "token": "eyJhbGciOiJIUzI1NiJ9.eyJvd25lciI6IjQiLCJhY3RvciI6IjQiLCJzY29wZXMiOiJST09UIiwiZXhwIjoxNzI5OTgyNDk4fQ.j068hm4oLFTFM2M5luS7UsB4YAEjYplkx1dAmubWVS8"
            }
        }
    }
}
```

As for the natural language style:

```
Use case "auth_root_account_implementation" execution with correlation ID of "509cbcbe-c6e8-4ffa-9ca6-62b1cd35e2e3" finished successfully. It took about 65 milliseconds. | Port insights: [FindRootAccountByLoginIdPortAdapter's insights: no exception has been thrown, FindRootAccountSecretPortAdapter's insights: no exception has been thrown, SessionTokenGenerationPortAdapter's insights: no exception has been thrown] [USE CASE INPUT]: { "pass": "**********", "loginId": "caertsc@capitolio.com" }; [USE CASE OUTPUT]: { "expiration": "3600", "name": "Capitólio", "id": "4", "token": "eyJhbGciOiJIUzI1NiJ9.eyJvd25lciI6IjQiLCJhY3RvciI6IjQiLCJzY29wZXMiOiJST09UIiwiZXhwIjoxNzMwMjQ3NDExfQ.kYGx3I8KbwzzRk9znYb-r6_h58359QVQZTFwFB9ipl8" };
```

The IO data processing for inclusion into the log payload is done natively with a _to-json_ method. During its execution, the cae-native process takes into account the fields of the IO objects that are marked with the ```@Sensitive``` annotation. Depending on the parameterized configuration of this annotation, the autolog will:

- Completely mask the field value
- Partially mask the field value
- Mask from right or from left
- Just ignore the actual value and put a fixed length of "*" characters

```java
@Getter
@Setter
public class SomeExample extends UseCaseInput {

    @Sensitive(unmaskFromLeft = false, unmaskedAmount = 3)
    private Long somePartiallyMaskedFieldFromRightToLeft;

    @Sensitive(unmaskedAmount = 5)
    private String anotherPartiallyMaskedFieldFromLeftToRight;

    @Sensitive(defaultMaskedAmount = 8)
    private String willJustBeAMaskWith8OfLength;

}

```

Any exceptions thrown during the execution of a Use Case will be intercepted by the Use Case itself. If the exception is a subtype of ```MappedException```, the Use Case instance will consider it a part of the designed flow, as it is a ```MappedException```, and let it go untouched. On the other hand, if it is not, the Use Case instance will see it as an unexpected exception and wrap it into a ```UseCaseExecutionException``` object. Either way the autolog will include this event in the log data.

##### ⤵ Auto input validation

Two types of Use Case accept input: the ```FunctionUseCase``` and the ```ConsumerUseCase```. Since they do, it is desirable to have a way to establish required input fields as _not-null_, _not-blank_, _not-empty_, etc. The cae-framework supports all of these, natively:

- ```@NotNullInputField```
- ```@NotBlankInputField```
- ```@NotEmptyInputField```
- ```@ValidInnerPropertiesInputField```

The input validation rule is established when any field of a ```UseCaseInput``` subtype is annotated with one or more of the above annotations.

They are designed as follows:

##### 📋 NotNullInputField
For fields of any type that must not be null.

##### 📋 NotBlankInputField
For ```String``` fields which can't be blank (empty or all-space strings).

##### 📋 NotEmptyInputField
For ```String``` and ```Collection``` fields that cannot be empty.

##### 📋 ValidInnerPropertiesInputField
For custom types that, inside, have their own properties with their own validation rules, based on the annotations mentioned above.

<br>

An example of ```UseCaseInput``` validation rule:

```java
@Getter
@Setter
public class AuthBotAccountUseCaseInput extends UseCaseInput {

    @NotNullInputField
    private Long rootAccountId;

    @NotNullInputField
    @NotBlankInputField
    private String passId;

    @NotNullInputField
    @NotBlankInputField
    private String passSecret;

}
```

That way, whenever the ```AuthBotAccountUseCase``` instance gets executed and receives an ```AuthBotAccountUseCaseInput``` object as input, the Use Case will internally call the ```UseCaseInput::validateProperties``` API, which will ensure the rule validation is respected. If it is, the Use Case accepts the input and proceeds to process it. If it is not, the Use Case rejects and throws an exception specifying what went wrong:

```
Field 'AuthRootAccountUseCaseInput:loginId' can't have blank values.
```

<br>

### →┊← Ports
Another key component of the framework is the concept of Ports. Ports act as the bridge between the core use case logic and external systems, such as databases or APIs. The CAE Framework supports four types of ports:

- ``✔️`` ``FunctionPort``: Receives input and returns output.
- ``✔️`` ``ConsumerPort``: Receives input but does not return output.
- ``✔️`` ``SupplierPort``: Returns output without input.
- ``✔️`` ``RunnablePort``: Executes without input or output.

<br>

### 🧅 Layers
The cae-framework expects the client application to be structured in 3 layers:

- **Core**
- **Adapters**
- **Assemblers**

This division can be applied to both monolithic and multilayered architectures. In a monolithic architecture, all layers are contained within a single project, with each layer organized into distinct packages. In a multilayered architecture, each layer is implemented as a separate project.

Regardless of the physical structure, the layers typically serve the following purposes:

- **Core**: Contains entities, use case logic, and port definitions. This is the business logic of the application, independent of external systems.
- **Adapters**: Handles interactions with the outside world, such as databases, APIs, or external services, ensuring the business logic remains isolated from implementation details.
- **Assemblers**: Responsible for creating and wiring use case instances from the core layer, injecting the necessary port adapters from the adapters layer to ensure proper integration and functionality.

It is recommended to use the [CLI tool](https://github.com/clean-arch-enablers-project/cae-cli) as it ensures the correct structure is provided.

<br>

...

<br>

## 🌐 Other components of the SDK:

- ``✔️`` [cae-cli](https://github.com/clean-arch-enablers-project/cae-cli)
- ``✔️`` [cae-utils-mapped-exceptions](https://github.com/clean-arch-enablers-project/cae-utils-mapped-exceptions)
- ``✔️`` [cae-utils-http-client](https://github.com/clean-arch-enablers-project/cae-utils-http-client)
- ``✔️`` [cae-common-primary-adapters](https://github.com/clean-arch-enablers-project/cae-common-primary-adapters)
- ``✔️`` [cae-utils-env-vars](https://github.com/clean-arch-enablers-project/cae-utils-env-vars)
- ``✔️`` [cae-utils-trier](https://github.com/clean-arch-enablers-project/cae-utils-trier)
- ``✔️`` [cae-rdb](https://github.com/clean-arch-enablers-project/cae-rdb)
- ``⏳`` [cae-service-catalog](https://github.com/clean-arch-enablers-project/cae-service-catalog)

<br>
<br>
<br>
<br>

<p align="center">
  CAE — Clean Architecture made easy.
</p>
