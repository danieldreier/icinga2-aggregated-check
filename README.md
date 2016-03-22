Icinga2 Check Aggregation Check
===============================

This is a nagios-style check that queries the Icinga 2 API to monitor the
results of other checks. The goal is to allow monitoring of logic like:

* warn when more than 1 app servers are critical, and go critical when 2 or more app servers are critical
* warn if fewer than 3 app servers are ok, and go critical if fewer than 2 app servers are ok

Usage
-----
You must have an erlang 18 executable available in your path. Depending on OS:
```shell
apt-get install erlang
```
```shell
brew install erlang
```

The following will submit the icinga2 service filter
`'"role::elasticsearch::data" in host.groups && service.name == "disk"'`.
It will warn if a minimum of 5 results are not in the `ok` state, and go
critical if a minimum of 3 results are not in the `ok` state.
```shell
./aggcheck --username icinga2 --password hunter2 --host icinga-master01-prod.example.com --service-filter '"role::elasticsearch::data" in host.groups && service.name == "disk"' --warn-threshold 5 --crit-threshold 3 --order min --state ok
```

For descriptions of all available options, run `./aggcheck` without any parameters or flags.

Known Limitations
-----------------

* does not gracefully handle timeouts
* most non-200 responses from the Icinga2 API will dump an ugly but readable erlang error
* there is no test coverage

Build Instructions
------------------

This project is written in erlang, and is built using the Mix build tool from
the elixir ecosystem. The easiest way to get a build environment is to follow
the install steps from http://elixir-lang.org/install.html.

The build output is an erlang escript file named "aggcheck", which is a single
self-contained file with all dependencies packaged inside, similar to a jar.

To build the escript executable:
```shell
mix deps.get
mix escript.build
```

To build a debian package, a Makefile is provided that uses FPM:
```shell
gem install fpm
Make
```


Contributing
------------

This is my first useful erlang code, so there are probably a lot of
opportunities for improvement. All such improvements / feedback about how I'm
doing it wrong are welcome.
