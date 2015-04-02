# Simple time tracking app for tasks

## Setup

Create an empty database:

    $ touch tasks.db

Start program:

    $ runhaskell tasks.hs

## Commands

**Logging tasks**

* `start` starts a task, e.g.:
        
        start Chat with Bob

    A task can be assigned to a case number by setting the first part of the description as a number:

        start 123 review code

* `stop` stops a started task and logs the time spent.
* `abandon` to abandon the current task.
* `rename` renames a started task. Like `start` but needs a current task, e.g.:

        start Chat with Bob
        rename 123 Chat with bob on code quality

* `again` start the last task again.

**Reporting**

* `current` shows current task.
* `last` shows last 10 tasks.
* `today` shows tasks done today.
* `yesterday` shows tasks done yesterday.
* `worked` hours worked today.
