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

All the files under a `usage-examples` directory are considered test files. To
exclude a file from the compilation, add its name to a file called
`.simple-tests-exclude` under the relevant `usage-examples` directory.

For instance, consider a library `someLib`. The directory structure is as
follows

```
openlilylib
└── ly
    └── someLib
        ├── __main__.ily
        └── usage-examples
            ├── .simple-tests-exclude
            ├── example1
            │   └── example.ly
            └── example2
                └── test.ly
```

to exclude `example1/example.ly` from the tests, the contents of
`.simple-tests-exclude` should be as follows

```
# Comments can be inserted using # chars
usage-examples/example1/example.ly
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
the following command (for Unix-like systems). The testing script will use
whatever version of LilyPond the command `lilypond` points to.

```
test/simple_tests.py
```

that will run the tests using whatever LilyPond version the command `lilypond`
points to. Example output:

```
Running LilyPond 2.18.2
OpenLilyLib directory /data/matteo/Development/lilypond/openlilylib
Found excludes file: /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/.simple-tests-exclude
Excluding /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/global.ily
Excluding /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/parts/alto-I.ily
Excluding /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/parts/basso-I.ily
Excluding /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/parts/soprano-I.ily
Excluding /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/parts/tenore-I.ily
Not a lilypond file, skipping /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/.simple-tests-exclude


Running test /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/example.ly
Command:  lilypond -I /data/matteo/Development/lilypond/openlilylib -I /data/matteo/Development/lilypond/openlilylib/ly /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/example.ly
------- OK! --------


Running test /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/main.ly
Command:  lilypond -I /data/matteo/Development/lilypond/openlilylib -I /data/matteo/Development/lilypond/openlilylib/ly /data/matteo/Development/lilypond/openlilylib/ly/gridly/usage-examples/multi-file/main.ly
------- OK! --------


Running test /data/matteo/Development/lilypond/openlilylib/ly/scholarly/usage-examples/annotate.ly
Command:  lilypond -I /data/matteo/Development/lilypond/openlilylib -I /data/matteo/Development/lilypond/openlilylib/ly /data/matteo/Development/lilypond/openlilylib/ly/scholarly/usage-examples/annotate.ly
------- OK! --------
===============================================================================

  0 failed tests out of 3

===============================================================================
```
