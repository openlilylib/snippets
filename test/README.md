Testing
=======

Since OpenLilyLib is a collection of interdependent libraries it is important to
ensure that changes to one library don't break another one. An approach to
mitigate this risk is to have a suite of automated tests, that can be run to
verify the integrity of OpenLilyLib as a whole.

[Travis CI](http://docs.travis-ci.com/) is a continuous integration service,
tightly coupled with Github, that is free for open source repositories. It
allows to automatically run test suites every time there is a push to the
repository. If any of the tests fail, then the owners of the repository will be
notified.

This directory contains a script (`simple_tests.py`) that recursively scans the
OpenLilyLib directory tree, looking for test cases. These test files are then
compiled with a configurable version of LilyPond. If the compilation exits with
a non-zero exit status, then the test is considered failed.

Test definition
---------------

Test files are thus normal LilyPond files, and can be the same files used as
usage examples. To mark one or more files as tests, so that `simple_tests.py`
can check them, simply create a `.simple-tests`, then list each file that should
be tested in its own line. Paths should be relative to `.simple-tests`.

For instance, consider a library `someLib`. The directory structure is as
follows

```
openlilylib
└── ly
    └── someLib
        ├── __main__.ily
        ├── .simple-tests
        └── usage-examples
            ├── example1
            │   └── example.ly
            └── example2
                └── test.ly
```

to mark `example1/example.ly` and `example2/test.ly` as tests, the contents of
`.simple-tests` should be as follows

```
# Comments can be inserted using # chars
usage-examples/example1/example.ly
usage-examples/example2/test.ly
```

LilyPond version configuration
------------------------------

LilyPond versions to be used for tests can be specified in `.travis.yml` in
OpenLilyLib's root directory. This is the configuration file for Travis
CI. Refer to the comments in that file for how to configure multiple
versions. If multiple versions are configured, then the continuous integration
service will run all the tests using all the specified versions. This way
problems specific to different LilyPond versions can be spotted in one go.

Running locally
---------------

The tests as specified by `simple_tests.py` can also be run locally, by issuing
the following command (for Unix-like systems)

```
test/simple_tests.py lilypond
```

that will run the tests using whatever LilyPond version the command `lilypond`
points to. Example output:

```
Running GNU LilyPond 2.18.2
OpenLilyLib directory /home/matteo/Development/openlilylib
Found file listing simple tests: /home/matteo/Development/lilypond/openlilylib/test/.simple-tests
Running test /home/matteo/Development/lilypond/openlilylib/test/example-test.ly  OK!
===============================================================================

  0 failed tests out of 1

===============================================================================
```
