# Invoicing

* Handle error properly while calling `Automaton`, perfect to pass them untouched with the help of latest `woody` release.
* Better and easier to compehend flow control in machines.
* More familiar flow control handling of machines, e.g. catching and wrapping thrown exceptions.
* Explicit stage denotion in the invoice machine?
* __Submachine abstraction and payment submachine implementation__.
* __Properly pass woody contexts around__.
* __Invoice access control__.

# Tests

* Fix excess `localhost` definitions (as soon as service discovery strategy will be finalized, hopefully).
* __Add generic albeit more complex test suite which covers as many state transitions with expected effects as possible__.
* Employ macros to minimize pattern matching boilerplate.
