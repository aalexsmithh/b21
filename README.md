# The b21 website

## Modules

This repo contains several web services and modules.

* `frontend`: houses the frontend code.

  These are just some static pages. The CSS and JavaScript live in their own
  directories `js` and `css`. Running `build-frontend.sh` concatenates them
  appropriately and copies them into `frontend`.

* `backend`: houses the backend web service.

  This is a Servant project for a simple HTTP API for any of dynamic services
  provided to the website. Presently, that includes the iCalendar bridging
  service, which converts events from the B21 Google Calendar into an internal
  format that is consumed by the frontend JavaScript code, which in turn adjusts
  the events for use with the CLNDR jQuery plugin we use to render the calendar.

* `debug`: this is the server to run to debug the site locally.

  The `debug.sh` script in this directory starts up the backend, compiles the
  frontend, and starts any auxiliary mock webservices necessary to run the
  site.

  The local site will be hosted on `http://localhost:8080/index.html`.

  *Slight quirk:* you do need to write out `/index.html` explicitly when testing
  the site.

* `iCalendar`: this is the iCalendar library that we use.
  It wasn't available on stackage, so we pull it in as a git submodule.

* `redirect-manager`: this is a webservice for updating the B21 redirects.

  The debugging server does not start this webservice.

## Running

### Preparation

Install the Haskell build tool `stack`.
Install the JavaScript package manager `npm`.
Run `stack setup` to install the Haskell compiler GHC.
Finally, run `stack build` in the project root. This will install all
dependencies and compile all the Haskell code.

### Running locally

`cd` into the `debug` directory and run `./debug.sh`. This will compile
frontend resources, compile the frontend, and lauch all necessary debugging
servers, namely:

  * the B21 webservice;
  * the debugging server which simulates nginx reverse-proxying API calls to
    the webservice, and other requests are tried to be served statically from
    the frontend directory.

If you update the HTML code under `frontend`, it should be unnecessary to re-run
the debugging server. However, if you edit the JavaScript or CSS, you will need
to run `./build-frontend.sh` or re-run the debugging server (which in turn calls
`./build-frontend.sh`).

## Miscellaneous

Calendar plugin:  https://github.com/kylestetz/CLNDR
