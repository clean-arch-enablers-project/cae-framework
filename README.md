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

Use Case types which accept input require the parameterized generic type of the input to be a subclass of ```UseCaseInput```, this way the Use Case can leverage the ```UseCaseInput::autoverify``` API for input validation rules. For the output types nothing is required.

<br>

#### ▶️ Use Case Execution
Every Use Case subtype will inherit the same API for getting executed:

```UseCase::execute```

The only differences among them are that some accept input and/or return output, while others do neither or only one of the two. Regardless, each type accepts the following parameter: an object of type ```ExecutionContext```. This object serves the purpose of identifying each request with a unique ID, so troubleshootings can rely on the execution context at the log level of analysis, for example. 

The ```ExecutionContext``` object keeps an attribute called ```correlationId``` which is the UUID that identifies each execution. It can be generated randomly or provided programmatically:

```java
//generating random correlationId
var random = ExecutionContext.ofNew();

//providing a previous set correlationId
var correlationId = UUID.randomUUID().toString();
var previouslyEstablished = ExecutionContext.of(correlationId)
```

The random approach serves well when the workflow begins at that point, but in case the flow starts at the frontend app, for example, it is interesting for the frontend app to generate a correlationId in UUID and pass it down to the backend service where the Use Case is gonna be executed and programmatically pass it as the correlationId of the Execution Context the Use Case will consume. This way the step-by-step can be monitored even throughout different applications of the stack.


When executed, a Use Case can have some side behaviors:

- ``✔️`` Autolog
- ``✔️`` Autoverify
- ``⏳`` Autocache
- ``✔️`` Autonotify
- ``✔️`` Autometrics
- ``✔️`` Autoauth with Scopes
- ``✔️`` Autoauth with RBAC

##### 📄 Autolog
Whenever an instance of use case gets executed, an automatic log will be generated. It can be in two modes:

- structured
- natural language

The structured mode is a JSON payload with the log data, while the natural language mode is basically a simple text. However, both can display the same data; they just differ in their presentation styles.

The contained info is:

- The use case: the name of the use case which is being executed (the object's class name)
- Execution correlation Id: an unique identifier per execution (can be parameterized or randomly generated as mentioned previously)
- Whether or not successful: if the flow didn't throw any exceptions
- Exception thrown (in case of unsuccessful executions): what went wrong
- Latency
- Port insights: insights of what's going on during the execution of each of the use case's ports
- IO data: what the use case execution received as input and what it returned as output

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

The IO data processing for inclusion into the log payload can be done natively by the framework. If that processing is delegated to the framework, it will take into account the fields marked with the ```@Sensitive``` annotation. Depending on the parameterized configuration of this annotation, the autolog will:

- Completely mask the field value into the log
- Partially mask the field value into the log
- Mask from right or from left into the log
- Just ignore the actual value and put a fixed length of "*" characters into the log

Example:

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
AutologProvider.SINGLETON.setProvidedInstance(LoggerAdapter.SINGLETON);
```

The ```AutologProvider``` is a native component of the cae-framework. The ```AutologProvider::setProvidedInstance``` will receive any implementation of the ```Logger``` interface.

Expanding on the usage of the ```AutologProvider``` API:

- ```AutologProvider::structuredFormat```: if set ```true```, the presentation style of the log payload is the JSON mentioned in the beginning of this section. If ```false```, it will be in a simple-text format.
- ```AutologProvider::setUseCasesLoggingIO```: another ```boolean``` for setting whether or not the autolog will include the IO data of Use Case executions.
- ```AutologProvider::setPortsLoggingIO```: same as the previous one, but for ```Ports``` (we'll get there).
- ```AutologProvider::setLoggingStackTrace```: whether or not the autolog will include logs of StackTrace for exceptions thrown during Use Case executions.
- ```AutologProvider::setNumberOfLinesFromStackTrace```: if the previous one is set ```true```, it is possible to set the number of StackTrace lines will be included into the log.
- ```AutologProvider::setIOLoggingMode```: whether to use the CAE Native mode (which converts objects to JSON) or to rely on the objects' ```toString``` implementations.

It will look like this:

```java
AutologProvider.SINGLETON
    .setProvidedInstance(LoggerAdapter.SINGLETON)
    .setIOLoggingMode(IOAutologMode.CAE_NATIVE)
    .structuredFormat(false)
    .setUseCasesLoggingIO(true)
    .setPortsLoggingIO(false)
    .setLoggingStackTrace(true)
    .setNumberOfLinesFromStackTrace(2);
```

<br>

##### ✅ Autoverify

Two types of Use Case accept input: the ```FunctionUseCase``` and the ```ConsumerUseCase```. Since they do, it is desirable to have a way to establish required input fields as _not-null_, _not-blank_, _not-empty_, etc. The cae-framework supports all of these, natively:

- ```@NotNullInputField```: for fields of any type that must not be null.
- ```@NotBlankInputField```: for ```String``` fields which can't be blank (empty or all-space strings) or ```Collection``` fields of ```String``` inner elements.
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

That way, whenever the ```AuthRootAccountUseCase``` instance gets executed and receives an ```AuthRootAccountUseCaseInput``` object as input, the Use Case will internally call the ```UseCaseInput::autoverify``` API, which will ensure the validation rule is applied. If it is, the Use Case accepts the input and proceeds to process it. If it is not, the Use Case rejects and throws an exception specifying what went wrong:

<br>

```
Field 'AuthRootAccountUseCaseInput:loginId' can't have blank values.
```

<br>

##### 📦 Autocache
...

<br>

##### 🔔 Autonotify
With this Autofeature it is possible to parameterize scenarios that will trigger notifications to a list of ```NotificationSubscriber``` instances, such as Email Service Clients, Custom Metrics Clients, etc., you decide.

It works like this:

First, declare at least 1 ```NotificationSubscriber``` implementation.
```java
public class DefaultNotificationObserver implements AutonotifySubscriber {

    public static final DefaultNotificationObserver SINGLETON = new DefaultNotificationObserver();

    @Override
    public void receiveNotification(Notification notification) {
        SimpleEmailService.SINGLETON.sendTeamNotificationEmail(notification);
    }

}
```
Then, provide it to the framework. You can provide as many as you see fit.
```java
public class StandaloneAutonotify {

    public static void startupSettings(){
        AutonotifyProvider.SINGLETON
                .setSubscriber(DefaultNotificationObserver.SINGLETON);
    }

}
```
Now it is just about parameterizing what scenarios must trigger new notifications.
```java
public class StandaloneAutonotify {

    //examples with all possibilities
    public static void startupSettings(){
        AutonotifyProvider.SINGLETON
                .considerNotAuthenticatedMappedExceptions()
                .considerNotAuthorizedMappedExceptions()
                .considerInputMappedExceptions()
                .considerNotFoundMappedExceptions()
                .considerInternalMappedExceptions()
                .considerMissingEnvVarExceptions()
                .considerNoRetriesLeftExceptions()
                .considerUnexpectedExceptions()
                .considerSpecifically(IOException.class)
                .considerSpecifically(RejectedExecutionException.class)
                .considerSpecifically(IllegalStateException.class)
                .considerSpecifically(...any specific type)
                .considerLatency(1000)
                .setSubscriber(DefaultNotificationObserver.SINGLETON);
    }

}
```
So any mentioned exceptions being thrown during the execution of any ```UseCase``` or ```Port``` instances, a ```Notification``` object will be delivered to all of your provided ```NotificationSubscriber``` instances. That includes latency threshold as well.

A ```Notification``` has the following schema:

```java
public class Notification{
    private final String subject; //has getter
    private final ExecutionContext executionContext; //has getter
    private final Exception exception; //has getter
    private final List<String> reasons; //has getter

    @Override
    public String toString() {
        return "Notification generated on '" +
                this.subject +"' during the execution of correlation ID '" +
                this.executionContext.toString() + "' because of the following reasons: " +
                SimpleJsonBuilder.buildFor(this.reasons);
    }
}
```

<br>

##### 📊 Autometrics
...

<br>

##### 🎯 Autoauth with Scopes
Use Case types annotated with the ```@ScopeBasedProtection``` will need to specify, within the annotation, the required scopes for being granted the access to execute the Use Case instance.

```java
@ScopeBasedProtection(scope = "ROOT || MAINTAINER")
public abstract class CreateUserAccountUseCase extends FunctionUseCase<
        CreateUserAccountUseCaseInput,
        CreateUserAccountUseCaseOutput> {}
```

For the Use Case above (```CreateUserAccountUseCase```), it is necessary to have either the _ROOT_ or the _MAINTAINER_ scope in order to execute it. The way the framework knows whether the responsible for the execution has the required scopes is via the ```Actor``` interface.

```java
public interface Actor {
    List<String> getScopes();
}
```

The idea is that, for example, in your REST API you deserialize the Bearer (_access token such as a JWT)_ you receive and decompose its scopes to your own ```Actor``` implementation.

An example of this, on the side of a client application:

```java
@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class ActorAdapter implements Actor {

    public static Actor ofAuthorizationHeader(String authorizationHeader){
        var plainJwt = Optional.ofNullable(authorizationHeader)
                .orElseThrow(() -> new NotAuthenticatedMappedException("No session provided"))
                .replace("Bearer ", "");
        var jwt = JWT.ofAccessToken(plainJwt);
        var claims = jwt.getDecryptedAccessToken();
        if (claims.getExpiration().before(new Date()))
            throw new NotAuthorizedMappedException("Session expired");
        return new ActorAdapter(claims.getOwner(), List.of(claims.getScopes()));
    }

    private final String id;
    private final List<String> scopes;

}
```

The example above extracts the expected ```scopes``` out of a JWT.

Once you've instantiated a new ```Actor``` implementation object, it is time to pass it to the ```ExecutionContext``` object that will be used in your ```UseCase``` execution.

For Use Case types which aren't annotated with ```@ScopeBasedProtection```, the ```ExecutionContext``` instance provided in each execution isn't required to have an instance of the ```Actor``` interface, however, for protected Use Case types, if one is not provided, the execution will be rejected and an exception will be thrown.

The way to provide an instance of ```Actor``` to the ```ExecutionContext``` is as follows, considering the example of ```ActorAdapter``` mentioned above:

```java
@PostMapping("/v1/enrollment-requests")
public ResponseEntity<ContentWrapper<CreateNewEnrollmentRequestUseCaseOutput>> execute(
      @RequestHeader(name = "Authorization") String authorization,
      @RequestHeader String correlationId,
      @RequestBody CreateNewEnrollmentRequestUseCaseInput input){
  var actor = ActorAdapter.ofAuthorizationHeader(authorization); // <-- instantiates Actor
  var context = ExecutionContext.of(correlationId, actor); // <-- passes to the ExecutionContext
  var output = this.useCase.execute(input, context); // <-- passes to the UseCase
  return ResponseEntity.status(201).body(ContentWrapper.of(output));
}
```

In the example above the ```Actor``` will only be authorized to execute the ```UseCase``` if it has the required scopes (considering such use case type uses ```ScopeBasedProtection```).

<br>

##### ⛑️ Autoauth with RBAC
...

<br>

All the ```Autofeatures``` mentioned so far are triggered at runtime, during the execution of Use Cases and their respective Ports. In addition, there's another ```Autofeature``` that runs during a different phase of the client application lifecycle: ```Autodoc```. More details below.

<br>

### 📖 Autodoc
During the build phase of your application, metadata from your domain logic is extracted into a file named ```cae-autodoc.json```. This serves as the foundation for the autodocumentation feature, which can be integrated with the upcoming ```CAE Real-Time Domain Catalog``` SaaS. The vision is to keep a live, up-to-date Domain Catalog as part of your CI/CD pipelines, allowing teams across the organization to stay in sync with all the available capabilities.

The ```Autodoc``` feature is fully agnostic to how functionalities are exposed: whether as REST endpoints, Kafka consumers, SQS listeners, CRON jobs, or any other primary adapter flavor. It focuses solely on the domain layer, enabling a unified and seamless way to document your application’s core logic.

<p align="center">
  (More details will be provided)
</p>


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
