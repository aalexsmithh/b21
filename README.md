# b21

## Running

Install the Haskell build tool "stack".
Run `stack setup` to install a compiler.
Finally, run `stack build` in the project root. This will install all
dependencies and compile all the Haskell code.
`cd` into the `debug` directory and run `./debug.sh`. This will compile
frontend resources, compile the frontend, and lauch all necessary debugging
servers, namely:

  * a python3 http.server instance to serve a static iCal file, simulating
    the Google Calendar backend;
  * the B21 webservice;
  * the debugging server which simulates nginx reverse-proxying API calls to
    the webservice, and other requests are tried to be served statically from
    the frontend.

Calendar plugin - https://github.com/kylestetz/CLNDR

B21 Logo Colours
Red: #FF1926
Green: #A9E5BB
Gray: #564D4A
Light Grey: #E9F0F2
