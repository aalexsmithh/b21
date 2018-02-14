#!/bin/bash

echo "starting ical server..."

if test -z "$B21_API_POST" ; then
    B21_API_PORT=8082
fi
export B21_API_PORT

if test -z "$B21_EMAIL_FILEPATH" ; then
    B21_EMAIL_FILEPATH=emails.txt
fi
export B21_EMAIL_FILEPATH

if test -z "$B21_CALENDAR_URI" ; then
    B21_CALENDAR_URI="http://127.0.0.1:8084/one-event.ics"
fi
export B21_CALENDAR_URI

echo "starting b21 webservice on port $B21_API_PORT"

exec stack exec b21-backend-exe
