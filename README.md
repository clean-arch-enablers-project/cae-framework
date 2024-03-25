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

[THIS README IS A WORK IN PROGRESS. IF YOU ARE HERE, PLEASE, WAIT FOR THE REST OF ITS DOCUMENTATION]
