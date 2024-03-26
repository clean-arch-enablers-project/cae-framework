# cae-framework
Welcome to the _CleanArchEnablers_ framework repository! The _cae-framework_ is open-source and meant to make the experience of developing software with clean architecture easier. Feel free to explore the source code and its documentation.
<br><br>

### 💡 The _Use Cases_ concept

The axis of this architecture is the Use Case concept: a system is not defined by being event driven, distributed in microservices, or anything of that nature. A system is defined by its use cases, how they are gonna be provided is another story. This is the premise. 

So, if a system is supposed to execute actions such as _updating customer score_, _creating new lead_ and _deleting inactive users_, it doesn't matter if these use cases are gonna be provided as REST API Endpoints, CRON jobs, etc. All that matters is:

- For each use case, will there be input and output? What are the contracts?
- Once a use case gets executed, what is going to be its workflow?

This is the frame of perspective which is going to define a software, according to the clean architecture literature. 

Once it is defined and implemented at the source code level, the next step is to engage in defining what is going to execute the use cases _(components called primary adapters)_ and what is going to provide for the use cases during their executions _(components called secondary adapters)_. It is only at that moment, after having built the use cases, that it matters whether or not they will end up being available as Kafka Topic Consumers, Spring MVC Endpoints, AWS Lambda Functions or whatever. It is only then that it matters if the database will be SQL or NoSQL, if the data will be retrieved from a REST API or directly from a database. 

When the use cases are built in a well defined manner, it is possible to reuse them in any flavor.

This concept is implemented by the _cae-framework_. Whenever a new use case is created, it will have one of the following types:

- FunctionUseCase
- ConsumerUseCase
- SupplierUseCase
- RunnableUseCase

It will depend on the kind of contract the use case has:

- Does it have input AND output? Then, it is a FunctionUseCase.
- Does it have ONLY input? In this case, it is a ConsumerUseCase.
- Does it have ONLY output? That is a SupplierUseCase.
- Does it NOT have input NOR output? This one is a RunnableUseCase.

The illustration below might help the visualization:
<br>

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/61ae50f2-be24-4713-8c8c-3294154154b5)

Some examples of possible Use Cases by type are:

- **SaveNewUser**: it will receive input (the payload with the new user's data to be persisted) and return some output (usually the ID of what has been created, in this case, the new user). That's a **FunctionUseCase**.
- **UpdateProduct**: it will receive input (the payload with the product's data to be updated). Once the update is done, usually it is not necessary to return anything. That's a **ConsumerUseCase**.
- **RetrieveLatestCompanyCreated**: it will return the newest company at the database. It doesn't need any input to get it going. So, that's a **SupplierUseCase**.
- **DeleteOldMessages**: it will delete old messages without having to receive input, nor it has to return any output. That's a **RunnableUseCase**.

Every example of Use Case mentioned above can be developed to be made available as REST API Endpoints, Queue Consumers, Topic Consumers, CRON jobs. You name it. If each Use Case is its own thing, it becomes a piece of software possible to be reused in a plug-in/plug-out fashion. In this manner a Use Case is **not** a REST API Endpoint, but instead is *dispatched* by one.

Take a look at some real examples.

![exem1](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/1590b936-8bb8-4947-95c6-491ce2318f4e)

That is a Java project. Each use case of it is located within the {groupId}.{artifactId}.core.use_cases package.

![exem2](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/f9e83fec-44fa-49c4-bdb9-0d9c64d792c5)

Each use case has its own package, following a way of the [Vertical Slice](https://www.google.com/search?q=vertical+slice+architecture&sca_esv=28795b6719ac1a08&rlz=1C1CHZN_pt-BRBR983BR983&sxsrf=ACQVn092nOQkKhjbLAOFpUwi8dW_x09svQ%3A1711451608581&ei=2K0CZpyCI9TZ1sQPie226Ac&ved=0ahUKEwiczreJ5pGFAxXUrJUCHYm2DX0Q4dUDCBA&uact=5&oq=vertical+slice+architecture&gs_lp=Egxnd3Mtd2l6LXNlcnAiG3ZlcnRpY2FsIHNsaWNlIGFyY2hpdGVjdHVyZTIKEAAYgAQYigUYQzIIEAAYgAQYywEyCBAAGIAEGMsBMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABEjIFlCqAVj-FXAEeAGQAQCYAbYBoAGtEaoBBDAuMTS4AQPIAQD4AQGYAhKgAo4SwgIKEAAYRxjWBBiwA8ICDRAAGIAEGIoFGEMYsAPCAgYQABgWGB7CAggQABgWGB4YD5gDAIgGAZAGCpIHBDQuMTSgB8hb&sclient=gws-wiz-serp) pattern. Inside each use case package, the same structure is used:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/f037cacf-7fe0-405e-be77-652e8a38d565)

It will always be the same:
- The Use Case contract (the class at the root level of the use case package)
- The Use Case I/O definitions (classes within the _io_ package)
- The Use Case implementation (classes within the _implementation_ package)
- The Use Case factory (class within the _factories_ package)

It all starts at the Use Case Contract level:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/c0ca2394-556c-467c-a13b-292ba2375f10)

Here the Use Case is declared as a FunctionUseCase, which means it'll have both Input and Output contracts. The IO is defined by the {UseCaseName}Input and the {UseCaseName}Output classes.

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/773e073b-f692-4814-8f3d-8e5af770f336)

At the right side of the image it is possible to observe how such contracts were defined. It determines the RetrieveCustomerUseCase will have as input:

- ownerId (required - thus the @NotNullInputField annotation)
- query (optional, but when present, can't be a blank string - thus the @NotBlankInputField annotation)
- asc (required)
- active (required)

And as output:

- customers

Now, if the contract of the RetrieveCustomerUseCase changes, it is possible not to make it a _breaking change_, once the IO is always the same: 

- RetrieveCustomersUseCaseInput
- RetrieveCUstomersUseCaseOutput

Its internal content might change, but the contract is final. So, if some new fields are added, it won't break the clients which consume the Use Case. If fields might be removed, it is possible to go with a deprecation policy instead of directly removing the fields, and no breaking change will be inflicted.

Once such contract is defined, it is possible to build the client code which will consume the Use Case. It would look like this:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/851f8f40-b14f-4ed4-a401-c94da21d0fe9)

Here it is a REST API Endpoint making the RetrieveCustomersUseCase available to be used. In order to get the use case to be executed, the Controller endpoint execution instantiated an object for the input (useCaseInput, line 27) and passed it at the execution method call (FunctionUseCase::execute, line 33). The execution call receives a second parameter as well, still not mentioned up to this moment. The correlation ID is a string value in UUID format. Use Case executions will always receive an instance of UseCaseExecutionCorrelation, which encapsulates the UUID string value. This is a design decision to generate logs at each Use Case execution with a unique identifier just out of the box. So every execution of any Use Case will have a log generated with a UUID identifier in it, telling if the Use Case execution was successful or not.

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/b328c58d-cbcf-410c-ad10-6d72e92bbcb5)

The value of the UUID is received as a parameter because if the whole lifecycle of a request that starts from the Frontend Web App is supposed to be easily traced, the same UUID would be used from there down to the backend service, so the UUID value is open to be given.

That's it about the Use Case contract. Now, what about its implementation? Take a look at the next image:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/037a03a1-493d-46a3-90a6-e0c35739fb1c)

Extremely simple, for this Use Case. The implementation class will inherit the method _applyInternalLogic_, which is supposed to wrap the internal workflow logic of the Use Case. Inside this scope the code is supposed to form a visual workflow of high abstraction steps. It is meant to be easily understandable, just by taking a look at it. In this specific instance, the workflow is very simple, because it is composed by only 1 step: make the query. Once it is done, the result is returned.

Now, take a look at another Use Case implementation example:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/8af67b14-9e64-49b9-aded-603d03a4567b)

It is very clear what are the steps:

- It increases the number of transactions the new customer has
- It activates the new customer
- It validates the new customer
- It stores the new customer at the persistence layer

How these steps are implemented is something one can find out entering each respective lower level. 

Take a look at another example of Use Case implementation:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/4b4ffc8e-9cf6-4243-ab54-024138efa6d2)

- It stores the new company
- It handles a new generic account for the new company
- It handles the first enrollment for that new company
- It handles the plan contract for the new company

If any further details are needed, just entering the lower level surely would be enough. With the whole picture being easily understandable, it is more likely to go down into the right precise chunk of code where the further details are located.

Now, how do the implementations interact with their respective dependencies? For instance, how the _RetrieveCustomersUseCaseImplementation_ retrieved the data from the persistence layer?

### 💡 The _Adapters_ Concept

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/704209bd-f0d3-4360-9722-d63508074a36)

When a Use Case implementation needs to interact with something that is external to its scope, it does it by interacting with abstractions. So if what's being actually interacted with changes, there'll be no coupling. The workflow is free from the peripheral parts of the system. Such abstractions are called _Ports_. They will define what the Use Case is able and willing to interact with, and the other side of it will have to do what's necessary to meet such requirements. The other side is the real dependency, like a HTTP Client to call an external API, or some Repository to manipulate data from a database. Though that's the case, a HTTP Client library will not know what are the specific requirements from Use Cases of a random project that is using it. That's where the _Adapter_ concept joins the conversation.

Adapters are meant to... adapt... _what_ the Use Cases know how to deal with to _what_ the real dependencies know how to deal with.

So the Ports are like slots, spaces to use plugins. The Adapters are the plugins, which will adapt the contract from the Ports to the Real Dependencies, and vice versa. The Real Dependencies are the real deal outside of the domain world: components such as Kafka Producers, Cloud Provider Clients, DAO repositories, etc.

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/3940cf6e-98a5-4ae2-bdc4-0673905bee65)

Let's take a look at how it is done with the _cae-framework_:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/cf3acb72-c2b3-48a7-9ee5-9414c6e32a31)

Considering the workflow, these are the steps:

- Map the input to an Entity object
- Increase the new customer number of transactions
- Activate the new customer
- Validate the new customer
- Store it at the persistence layer

Which of these steps seems like something out of the domain layer? 

Increasing the number of transactions is something that looks like a business rule. If one guessed that, the guess would be correct. Once that is the case, such logic could be located within an Entity, and that's exactly what it is. The "addNewTransaction" (line 30) is a method from an Entity called Customer. That's why it was necessary to map the input object (SaveNewCustomerUseCaseInput) to the Entity format in the first place: to be able to use the Entity's API. Once the Customer object is created, it is possible to use its methods with its business rules. 

- Customer::addNewTransaction
- Customer::activate
- Customer::validate

How these methods are implemented, what their business rules are, that's within the Entity scope.

Once the business rules are applied, it is time to apply the application rule of storing what's being generated at a persistence layer.

That's the part of the workflow where external components are needed. To represent them, an abstraction is created: the _StoreNewCustomerPort_.

Since it is a dependency which will be injected, it is declared as a global attribute (line 14), which will be initialized at the constructor, when the Use Case implementation object is instantiated (line 22).

That's how the Port component looks like:

![image](https://github.com/clean-arch-enablers-project/cae-framework/assets/60593328/a4396290-85c0-47fa-a06f-1f3b1142348f)

It is just an abstract class that extends a kind of _Port_ from the _cae-framework_.

Just like the Use Case types, the Port types come in four:

- FunctionPort (I/O)
- ConsumerPort (I)
- SupplierPort (O)
- RunnablePort

Once a class inherits one of these types, it will come with an execution method. It will always receive the UseCaseExecutionCorrelation parameter, and depending on its base type, a generic typed input. Just like the Use Case types contract.

[THIS README IS A WORK IN PROGRESS. IF YOU ARE HERE, PLEASE, WAIT FOR THE REST OF ITS DOCUMENTATION]
