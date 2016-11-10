===============
Maven scripting
===============

Support for scripting tasks on Maven POM files (and IDE project files), using yaidom. Maven POM files are
modeled using a "yaidom dialect".

Typical scripting tasks made easy by this project include:

* Querying a POM file for absence of snapshot dependencies
* Querying several POM files for the same versions of some of their dependencies

This project has no knowledge about effective POM files (yet?).
