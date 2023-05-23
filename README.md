proleap-cobol
=============

Code base for proleap-cobol that allows you to analyze, execute & transform COBOL in the cloud.

You find a deployed version of this repository at [proleap.io](https://www.proleap.io/).


Eclipse
-------

- Install Eclipse >= 2022‑12
- Open Eclipse > Preferences > Maven > Annotation Processing and select "Automatically configure JDT APT".
- Open "Import Projects" > Maven > Existing Maven Projects
- Browse root directory to repository `proleap-cobol` and click Finish


Dependency proleap-cobol-parser
-------------------------------

* Clone repository `https://github.com/uwol/proleap-cobol-parser` locally.
* As described in the README.md of the repository, run:

```
$ mvn clean install
```

* Now your local `~/.m2/repository` should contain a build of dependency `proleap-cobol-parser`.
* In Eclipse, open "Import Projects" > Maven > Existing Maven Projects
* Browse root directory the repository `proleap-cobol-parser` and click Finish


Build Process
-------------

The build process is based on Maven (version 3 or higher). Building requires a JDK 17.

Install homebrew.

```
$ echo export "JAVA_HOME=\$(/usr/libexec/java_home)" >> ~/.zshrc
$ echo export "eval $(/opt/homebrew/bin/brew shellenv)" >> ~/.zshrc
```

```
$ brew install maven
$ mvn -T 1C clean test
```

```
$ mvn clean package
```
