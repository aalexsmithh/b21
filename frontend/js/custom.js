'use strict';

$(function() {
    // fetch the calendar of events from the API
    $.get({
        url: '/api/get_calendar',
        dataType: 'json',
    })
    // if that succeeds, then we render the calendar
    .done(function(res) {
        $('#calendar').clndr({
            events: _(res).map(function(event) {
                // convert the event time from UTC to localtime, and throw away
                // the time info to just extract the date.
                event.date = moment(event.date).local().format('YYYY-MM-DD');
                return event;
            }),
            template: $('#calendar-template').html(),
        });
    })
    // if it fails then we log the error and say sorry.
    .fail(function(res) {
        console.log(res);
        $('#calendar').text('Unable to load the calendar.');
    });
});
