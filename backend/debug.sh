#!/bin/bash

echo "starting ical server..."

(cd ../ics ; python3 -m http.server 8084) &

if test -z "$B21_API_POST" ; then
    export B21_API_PORT=8082
fi
export B21_EMAIL_FILEPATH=emails.txt
export B21_CALENDAR_URI="http://localhost:8084/one-event.ics"

echo "starting b21 webservice on port $B21_API_PORT"

exec stack exec b21-backend-exe
