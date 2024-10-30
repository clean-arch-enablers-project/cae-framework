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

That's how you declare Use Cases:

```java
//will receive objects of type GetBotAccountsUseCaseInput and return ones of type GetBotAccountUseCaseOutput
public abstract class GetBotAccountsUseCase extends FunctionUseCase<
        GetBotAccountsUseCaseInput,
        GetBotAccountsUseCaseOutput> {}
```

```java
//will receive objects of type UpdateUserAccountProfileUseCaseInput and return nothing
public abstract class UpdateUserAccountProfileUseCase extends ConsumerUseCase<UpdateUserAccountProfileUseCaseInput> {}
```

```java
//wont receive anything as input, but will return an object of type GetUserAccountProfilesUseCaseOutput
public abstract class GetUserAccountProfilesUseCase extends SupplierUseCase<GetUserAccountProfilesUseCaseOutput> {}
```

```java
//neither receives or returns anything; it only executes something
public abstract class DeleteInactiveLeadsUseCase extends RunnableUseCase {}
```

Use Case types which accept input require the parameterized generic type of the input to be a subclass of ```UseCaseInput```, this way the Use Case can leverage the ```UseCaseInput``` API for input validation rules. For the output types nothing is required.


#### ▶️ Use Case Execution
Every Use Case subtype will inherit the same API for getting executed:

```UseCase::execute```

The difference between them all is only that some accept input and/or return output and others don't do either or at least one of the options. Regardless, everyone of them accepts the following parameter: an object of type ```ExecutionContext```. This object serves the purpose of identifying each request with a unique ID, so troubleshootings can rely on the execution context at the log level, for example. 

The ```ExecutionContext``` object keeps an attribute called ```correlationId``` which is the UUID that identifies each execution, it can be generated randomly or provided programmatically:

```java
//generating random correlationId
var random = ExecutionContext.ofNew();

//providing a previous set correlationId
var correlationId = UUID.randomUUID().toString();
var previousEstablished = ExecutionContext.of(correlationId)
```

The random approach serves well when the workflow begins at that point, but in case the flow starts at the frontend app, for example, it is interesting for the frontend app to generate a correlationId in UUID and pass it down to the backend service where the Use Case is gonna be executed and programmatically pass it as the correlationId of the Execution Context the Use Case will consume. This way the how step-by-step can be monitored even throughout different applications of the stack.


When executed, a Use Case can have some side behaviors:

- ``✔️`` Autolog
- ``✔️`` Auto input validation
- ``⏳`` Autocache
- ``⏳`` Autonotify
- ``✔️`` Scope based authorization validation
- ``⏳`` Role based authorization validation

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

The framework itself doesn't have a dependency for an actual logger, it depends on the client application to provide an implementation of the ```Logger``` native interface:

```java
public interface Logger {

    void logInfo(String info);
    void logError(String error);
    void logDebug(String info);

}
```

An example of an actual implementation of the interface above, on the side of a client application:

```java
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class LoggerAdapter implements Logger {

    public static final Logger SINGLETON = new LoggerAdapter();

    @Override
    public void logInfo(String info) {
        log.info(info);
    }

    @Override
    public void logError(String error) {
        log.error(error);
    }

    @Override
    public void logDebug(String info) {
        log.debug(info);
    }

}
```

Once an implementation of the ```Logger``` interface is created, to provide it to the framework, it goes like this:

```java
LoggerProvider.SINGLETON.setProvidedInstance(LoggerAdapter.SINGLETON);
```

The ```LoggerProvider``` is a native component of the cae-framework. The ```LoggerProvider::setProvideInstance``` will receive any implementation of the ```Logger``` interface.

Expanding on the usage of the ```LoggerProvider``` API:

- ```LoggerProvider::structuredFormat```: if set ```true```, the apresentation style of the log payload is the JSON mentioned in the beginning of this section. If ```false```, in a simple text format.
- ```LoggerProvider::setUseCasesLoggingIO```: another ```boolean``` for setting whether or not the autolog will include the IO data of Use Case executions.
- ```LoggerProvider::setPortsLoggingIO```: same as the previous one, but for ```Ports``` (we'll get there).
- ```LoggerProvider::setLoggingStackTrace```: whether or not the autolog will include logs of StackTrace for exceptions thrown during Use Case executions.
- ```LoggerProvider::setNumberOfLinesFromStackTrace```: if the previous one is set ```true```, it is possible to set the number of StackTrace lines will be included into the log.
- ```LoggerProvider::setIOLoggingMode```: whether to use the CAE Native mode (which converts objects to JSON) or to rely on the objects' ```toString``` implementations.

It will look like this:

```java
LoggerProvider.SINGLETON
    .setProvidedInstance(LoggerAdapter.SINGLETON)
    .setIOLoggingMode(IOLoggingMode.CAE_NATIVE)
    .structuredFormat(false)
    .setUseCasesLoggingIO(true)
    .setPortsLoggingIO(false)
    .setLoggingStackTrace(true)
    .setNumberOfLinesFromStackTrace(2);
```

<br>

##### ⤵ Auto input validation

Two types of Use Case accept input: the ```FunctionUseCase``` and the ```ConsumerUseCase```. Since they do, it is desirable to have a way to establish required input fields as _not-null_, _not-blank_, _not-empty_, etc. The cae-framework supports all of these, natively:

- ```@NotNullInputField```: for fields of any type that must not be null.
- ```@NotBlankInputField```: for ```String``` fields which can't be blank (empty or all-space strings).
- ```@NotEmptyInputField```: for ```String``` and ```Collection``` fields that cannot be empty.
- ```@ValidInnerPropertiesInputField```: for custom types that, inside, have their own properties with their own validation rules, based on the annotations mentioned above.

The input validation rule is established when any field of a ```UseCaseInput``` subtype is annotated with one or more of the above annotations.

<br>

An example of ```UseCaseInput``` validation rule:

```java
@Getter
@Setter
public class AuthRootAccountUseCaseInput extends UseCaseInput {

    @NotNullInputField
    @NotBlankInputField
    private String loginId;

    @NotNullInputField
    @NotBlankInputField
    @Sensitive
    private String pass;

}
```

That way, whenever the ```AuthRootAccountUseCase``` instance gets executed and receives an ```AuthRootAccountUseCaseInput``` object as input, the Use Case will internally call the ```UseCaseInput::validateProperties``` API, which will ensure the validation rule is respected. If it is, the Use Case accepts the input and proceeds to process it. If it is not, the Use Case rejects and throws an exception specifying what went wrong:

<br>

```
Field 'AuthRootAccountUseCaseInput:loginId' can't have blank values.
```

<br>

##### 📦 Autocache
...

<br>

##### 🔔 Autonotify
...

<br>

##### 🎯 Scope based authorization
Use Case types annotated with the ```@ProtectedUseCase``` will need to specify, within the annotation, the required scopes for being granted the access to execute the Use Case instance.

```java
@ProtectedUseCase(scope = "ROOT || MAINTAINER")
public abstract class CreateUserAccountUseCase extends FunctionUseCase<
        CreateUserAccountUseCaseInput,
        CreateUserAccountUseCaseOutput> {}
```

For the Use Case above (```CreateUserAccountUseCase```), it is necessary to have either the _ROOT_ or the _MAINTAINER_ scope in order to execute it. The way the framework knows whether or not the responsible for the execution has the required scopes is via the ```Actor``` interface.

```java
public interface Actor {
    List<String> getScopes();
}
```

An example for an actual implementation of it, on the side of a client application:

```java
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class ActorSessionManager implements Actor {

    @Getter
    private final String owner;
    @Getter
    private final String id;
    private final String scopes;

    public static Actor createOutta(String authorizationHeader){
        var jwt = JWT.of(authorizationHeader.replace("Bearer ", ""));
        var claims = jwt.getDecryptedJWT();
        if (claims.getExpiration().before(new Date()))
            throw new UnauthorizedException();
        return new ActorSessionManager(
                claims.getOwner(),
                claims.getActor(),
                claims.getScopes()
        );
    }

    @Override
    public List<String> getScopes() {
        return List.of(this.scopes);
    }
```

The example above extracts the expected ```scopes``` out of a JWT.

Once a concrete implementation of the ```Actor``` interface is created, the way to provide its instances on each Use Case execution is via the ```ExecutionContext``` object. For Use Case types which aren't annotated with ```@ProtectedUseCase```, the ```ExecutionContext``` instance provided in each execution isn't required to have an instance of the ```Actor``` interface, however, for protected Use Case types, if one is not provided, the execution will be rejected.

The way to provide an instance of ```Actor``` to the ```ExecutionContext``` is as follows, considering the example of ```ActorSessionManager``` mentioned lastly as the implementation:

```java
var actor = ActorSessionManager.createOutta(authorization);
var executionContext = ExecutionContext.of(correlationId, actor);
var useCaseOutput = useCase.execute(useCaseInput, executionContext);
```

<br>

##### ⛑️ Role based authorization
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
