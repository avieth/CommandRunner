## Overview

This is a repository for an experiment in correct, robust software. The goal is
to facilitate the expression of a program which features undo/redo, in such a
way that we can prove the reliability of this feature.

## Current status

We have an example application in which a drawing can be modified via two
commands which are mutually inverse. It's easy to verify that the commands, if
executed in sequence on the same state data, will leave that data fixed, and
from this I can say with confidence that undo/redo will always work as
expected. Of course, we have no concurrency, and the program is so incredibly
simple, so that's not such a remarkable fact, but it's a start.
