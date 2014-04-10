HsOptions Examples
==================
HsOptions is a library for Haskell to interpret command line arguments and configuration files as flags.
Multiple modules can define their flags independently and can be composed together easily,
also help text and usage text is handled automatically.


Requirements
----
Follow the *How to build* section and make sure the library compiles correctly.


Usage
----
There are several examples, these are common use cases for each:

**SimpleFlag.hs**
```
./dist/build/SimpleFlag/SimpleFlag --user_id 1234
```

