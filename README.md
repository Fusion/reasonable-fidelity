# Reasonable Fidelity

## What?

The idea behind this app is to record whatever actions you would when testing a  
web app, then replay them later and compare their level of fidelity. 
Attributes/files you do not wish to compare can be added to 'ignore' files.

### How?

- Open your favorite browswer (let's pretend it's Chrome)
- Open the developer console
- Open the 'Network' tab and select 'Preserve logs'
- Interact with application
- Right click the developer console, select 'Save as HAR with content'
- Use this file to replay and compare
- Fine tune ignored attributes in config/ directory

### Some Features

- CSV or full text output
- Multiple configuration environments
- Cookies or dedicated token
- Plugins (written in Python)
- ...

## TODO

- more advanced attribute extractors
- more configurable login
- Point Free composition!

## Using

```
Valid Arguments:

help:
    display help

sanity:
    check requirements & help explain issues

run:
    run tests
    options:
        --csv: output csv data rather than plain text
         -source <file_name>: specify alternate .har file
         -config <dir_name>: specify alternate config directory
         -forcelogin: attempt log in to service rather than trust cookies/token (experimental!)

modified:
    show which lines were changed from the reference capture

timestamp <startedDateTime> <time>:
    return a timestamp for 'start_at'/'stop_at' configuration

edit <filename>:
    edit har file -- for now, mark actions for deletion

reset:
    re-create default configuration file
```

## Building

From scratch, really...

OS X
```bash
brew install opam
```
Linux
```bash
sudo apt install m4
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```
Then (Common)
```bash
opam init
opam update && opam switch 4.05.0
opam install reason lwt tls cohttp cohttp-lwt-unix yojson config-file lymp curses extlib
```

## Specifics of this code

My first ReasonML app! So, yeah, it's not perfect,

### Things to keep in mind if this is your first use of Lwt promises

- Lwt is our Promises library
- Lwt comes with infix operators:
- `>>=` equivalent to Lwt.bind
- `>|=` equivalent to Lwd.map (right side does not return a promise)
- '=<<` and `=|<` are reversed versions
