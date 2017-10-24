#!/bin/bash

B21_PORT=8082 \
B21_EMAIL_FILEPATH=emails.txt \
B21_CALENDAR_URI="https://calendar.google.com/calendar/ical/s57ukja5ghqsuqmtiifge3283g%40group.calendar.google.com/public/basic.ics" \
exec stack exec b21-backend-exe
