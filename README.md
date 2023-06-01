ProLeap ANTLR4-based analyzer, interpreter & transformer for COBOL
==================================================================

This is an analyzer, interpreter & transformer for COBOL based on [ProLeap COBOL parser](https://github.com/uwol/proleap-cobol-parser). The parser generates an **Abstract Syntax Tree** (AST) and **Abstract Semantic Graph** (ASG) for COBOL code, which then is processed by this analyzer, interpreter & transformer.

At [proleap.io](https://www.proleap.io/) you find a deployed version of this repository that allows you to test with your COBOL files.

💫 **Star** if you like our work.

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![ProLeap on Twitter](https://img.shields.io/twitter/follow/proleap_io.svg?style=social&label=Follow)](https://twitter.com/proleap_io)


Getting started
---------------

In Eclipse open [ProLeapCobolEmbeddedJetty](proleap-cobol-app/src/main/java/io/proleap/cobol/ProLeapCobolEmbeddedJetty.java) and run the main method as Java application. A Jetty will start, which offers the functionality as on [proleap.io](https://www.proleap.io/).


Support
-------

This program is free software under AGPL v3 license and in this form comes without support. However, our company [ProLeap GmbH](https://www.proleap.io/imprint) is open to discuss project and license requirements with your company. Feel free to contact us at contact@proleap.io


License
-------

This program is free software under AGPL v3 license. So source code generated from this program is [NOT subject to AGPLv3 licensing](https://www.gnu.org/licenses/gpl-faq.html#WhatCaseIsOutputGPL). You can use this program to transform COBOL to Java and keep the copyright for your code.

The AGPLv3 license is only meant for this program itself. Our idea is that a joint effort in the industry is needed to build an open source tooling for COBOL modernization. Hopefully, future source code contributions from the industry are flowing back into this program, making COBOL modernization increasingly feasible for companies.


Where to look next
------------------

- [ANTLR4 COBOL grammar](https://github.com/uwol/proleap-cobol-parser/tree/master/src/main/antlr4/io/proleap/cobol/Cobol.g4)
- [ANTLR4 COBOL preprocessor grammar](https://github.com/uwol/proleap-cobol-parser/tree/master/src/main/antlr4/io/proleap/cobol/CobolPreprocessor.g4)
- [Analysis unit tests](proleap-cobol-analysis/src/test/java/io/proleap/cobol/analysis)
- [Interpreter unit tests](proleap-cobol-interpreter/src/test/java/io/proleap/cobol/interpreter)
- [Transform unit tests](proleap-cobol-transform/src/test/java/io/proleap/cobol/transform)


Build dependency proleap-cobol-parser
-------------------------------------

* Clone repository [uwol/proleap-cobol-parser](https://github.com/uwol/proleap-cobol-parser) locally.
* As described in the README.md of that repository, run:

```
$ mvn clean install
```

* Now your local `~/.m2/repository` should contain dependency `io/github/uwol/proleap-cobol-parser`.
* In [Eclipse](https://eclipse.org) import the directory `proleap-cobol-parser` as a an `existing Maven project`.


Build Process
-------------

The build process is based on Maven (version 3 or higher). Building requires a JDK 17.

* Clone or download the repository.
* In [Eclipse](https://eclipse.org) import the directory as a an `existing Maven project`. *Important*: Open Eclipse > Preferences > Maven > Annotation Processing and select "Automatically configure JDT APT", so that dependency injection framework [Micronaut](https://micronaut.io/) automatically processes all `@Inject` annotations.
* To build, run:

```
$ mvn clean package
```

* The test suite executes tests against COBOL test code.
* You should see output like this:

```
[INFO] Scanning for projects...
...
[INFO] ------------------------------------------------------------------------
[INFO] Reactor Summary for proleap-cobol 1.0.0:
[INFO] 
[INFO] proleap-cobol ...................................... SUCCESS [  0.066 s]
[INFO] proleap-cobol-commons .............................. SUCCESS [  3.431 s]
[INFO] proleap-cobol-analysis ............................. SUCCESS [  6.682 s]
[INFO] proleap-cobol-interpreter .......................... SUCCESS [  4.164 s]
[INFO] proleap-cobol-transform ............................ SUCCESS [  4.180 s]
[INFO] proleap-cobol-app .................................. SUCCESS [ 14.306 s]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
```

* To install the JAR in your local Maven repository:

```
$ mvn clean install
```

* To only run the tests:

```
$ mvn clean test
```
