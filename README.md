# b21

## Modules

This repo contains several web services and modules.

  * `frontend`: houses the frontend build system.
    This is a Hakyll project for building the static content of the site,
    coupled with a Makefile to download and compile static resources such as
    JavaScript dependencies and CSS.

  * `backend`: houses the backend web service.
    This is a Servant project for a simple HTTP API for any of dynamic services
    provided to the website.

  * `debug`: this is the server to run to debug the site locally.
    The `debug.sh` script in this directory starts up the backend, compiles the
    frontend, and starts any auxiliary mock webservices necessary to run the
    site.

  * `iCalendar`: this is the iCalendar library that we use.
    It wasn't available on stackage, so we pull it in as a git submodule.

  * `redirect-manager`: this is a webservice for updating the B21 redirects.

## Running

### Preparation

Install the Haskell build tool `stack`.
Install the JavaScript package manager `yarn`.
Run `stack setup` to install the Haskell compiler GHC.
Finally, run `stack build` in the project root. This will install all
dependencies and compile all the Haskell code.

### Running locally

`cd` into the `debug` directory and run `./debug.sh`. This will compile
frontend resources, compile the frontend, and lauch all necessary debugging
servers, namely:

  * a python3 http.server instance to serve a static iCal file, simulating
    the Google Calendar backend;
  * the B21 webservice;
  * the debugging server which simulates nginx reverse-proxying API calls to
    the webservice, and other requests are tried to be served statically from
    the frontend.

## Miscellaneous

Calendar plugin:  https://github.com/kylestetz/CLNDR

B21 Logo Colours:

  *  Red: `#FF1926`
  *  Green: `#A9E5BB`
  *  Gray: `#564D4A`
  *  Light Grey: `#E9F0F2`
