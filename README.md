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
opam update && opam switch 4.09.0
```

To install dependencies .opam file from dune-project:

```bash
dune build @install
```

### Curses

The built-in editor relies on ncurses (v5). On some Linux distros, you may have to run `make prepare_curses`

### More building...

Same goes for lymp that is running cold that's a bit too... dated.

Run:

```bash
make prepare
```

When everything is built properly, we will want to create symlinks to clean stubs:

```bash
sudo bash -c 'for lib in ~/.opam/default/lib/stublibs/*; do libname="$(basename $lib)"; ln -s $lib /usr/local/lib/${libname/dll/lib}; done'
```

## Specifics of this code

My first ReasonML app! So, yeah, it's not perfect,

### Things to keep in mind if this is your first use of Lwt promises

- Lwt is our Promises library
- Lwt comes with infix operators:
- `>>=` equivalent to Lwt.bind
- `>|=` equivalent to Lwd.map (right side does not return a promise)
- '=<<` and `=|<` are reversed versions

### Misc Notes

Parallel queries

- We can use Lwt.run(Lwt.join(...
- execute.get is good
- we could run up to n threads in parallel
- ...or until we meet execute.post
- All pending threads to be stored in a list...
- Orchestrated in execute.get?
- Q: what to do about pause when > 0.?
- Do something about non-csv output, which is broken up

- {action, url, query_string, post_data)
