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

##### ꩜ Use Case Correlation
The ``UseCaseExecutionCorrelation`` class generates a unique identifier (UUID) for each use case execution, facilitating enhanced tracking and logging. Additionally, it supports the optional inclusion of an ``Actor``, enabling role-based authorization for protected use case instances.

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

## 🪄 Capabilities

### 🛰️ Satellites
Satellites are auxiliary components that "orbit" around the core use cases, adding features such as logging, caching, and monitoring without interfering with core logic. This promotes adherence to the Open/Closed principle of SOLID, allowing functionality to extend without modifying the core system.

Each use case has its own processor, acting as a central hub (or proxy area) for integrating satellites. This design promotes a clean separation of concerns, allowing additional features to be easily plugged in. The current satellites include:

- ``✔️`` **Input Validation**: Ensures that inputs comply with predefined rules and standards.
- ``✔️`` **Exception Handling**: Differentiates between expected, mapped exceptions and unexpected errors, handling each appropriately.
- ``✔️`` **Logging**: Automatic logging through Logger and UseCaseLoggingManagement for comprehensive tracking of operations.
- ``✔️`` **Authorization**: Enforces access control based on user-defined scopes, maintaining security across use cases.

Upcoming satellites:

- ``⏳`` **Caching**: Improves performance by fetching results from caching systems, bypassing execution for repetitive use case instances.
- ``⏳`` **Notification**: Sends automatic notifications when exceptions occur, with customizable alerts for specific scenarios.

<br>

### 📄 Auto-Documentation
``✔️`` The CAE Framework includes automatic documentation generation. The ``UseCaseDocumentationExternalizer`` gathers metadata for all use cases during the build phase, producing the ``cae-docfile.json`` file that can be used to track available use cases across the system.

<br>

### 🔎 Auto-Logging
``✔️`` Logging in the CAE Framework is decoupled from the core logic by using the ``Logger`` interface. This allows developers to implement their preferred logging mechanism without coupling it to the framework.

The ``LoggerProvider`` class centralizes logging configuration, allowing for flexible log handling, including:

- ``✔️`` **Enable/Disable Input/Output Logging**: Configure whether input and output data should be included in the logs.
- ``✔️`` **Synchronous/Asynchronous Logging**: Choose between non-blocking asynchronous logging or standard synchronous logging.
- ``✔️`` **Structured vs. Simple Log Formatting**: Select between structured JSON logs or simple natural language log entries.

<br>

## 🧩 Putting it together
The basic workflow for the development process is as follows:

- **Define a Use Case**: Extend one of the UseCase classes (``Function``, ``Consumer``, ``Supplier``, or ``Runnable``) to implement your core business logic.
- **Implement Ports**: Connect your core logic to external systems by utilizing ``Function``, ``Consumer``, ``Supplier``, or ``Runnable`` ports.
- **Set Metadata and Validation**: Leverage annotations to enforce input validation and authorization requirements effortlessly.
- **Logging**: Leverage built-in logging for transparent and traceable use case execution.
- **Automatic Documentation**: Benefit from auto-generated documentation for all use case instances during the build process of your application.
- **Execute**: Use the built-in processors to enjoy automatic handling of logging, validation, and error management during execution.

<br>

## 💡 Tutorials
Tutorials will soon be available on the SDK's YouTube channel: [Clean Arch Enablers SDK](https://www.youtube.com/@CleanArchEnablersSDK).

<br>

## 🌐 Other components of the SDK:

- ``✔️`` [cae-cli](https://github.com/clean-arch-enablers-project/cae-cli)
- ``✔️`` [cae-utils-mapped-exceptions](https://github.com/clean-arch-enablers-project/cae-utils-mapped-exceptions)
- ``✔️`` [cae-utils-http-client](https://github.com/clean-arch-enablers-project/cae-utils-http-client)
- ``✔️`` [cae-common-primary-adapters](https://github.com/clean-arch-enablers-project/cae-common-primary-adapters)
- ``✔️`` [cae-utils-env-vars](https://github.com/clean-arch-enablers-project/cae-utils-env-vars)
- ``✔️`` [cae-utils-trier](https://github.com/clean-arch-enablers-project/cae-utils-trier)
- ``⏳`` [cae-service-catalog](https://github.com/clean-arch-enablers-project/cae-service-catalog)

<br>
<br>
<br>
<br>

<p align="center">
  CAE — Clean Architecture made easy.
</p>
