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

## TODO

- more advanced attribute extractors
- more configurable login
- cookies, beyond token

## Setup

```bash
brew install opam
opam init
opam update && opam switch 4.03.0
```

## Specifics of this code

My first ReasonML app! So, yeah, it's not perfect,

### Things to keep in mind if this is your first use of Lwt promises

- Lwt is our Promises library
- Lwt comes with infix operators:
- `>>=` equivalent to Lwt.bind
- `>|=` equivalent to Lwd.map (right side does not return a promise)
- '=<<` and `=|<` are reversed versions
