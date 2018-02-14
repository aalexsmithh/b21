niceTimeFormat = 'h:mm a';

$(function () {
    // fetch the calendar of events from the API
    $.get({
        url: '/api/get_calendar',
        dataType: 'json',
    })
        // if that succeeds, then we render the calendar
        .done(function(res) {
            console.log(JSON.stringify(res));
            $('#calendar-widget-container').clndr({
                events: _(res).map(function(event) {
                    // convert the event time from UTC to localtime, and throw
                    // away the time info to just extract the date.
                    event.date = moment.utc(event.startDate);
                    event.startDate = moment.utc(event.startDate);
                    event.endDate = moment.utc(event.endDate);
                    return event;
                }),
                template: $('#calendar-template').html(),
                showAdjacentMonths: false,
            });
        })
        // if it fails then we log the error and say sorry.
        .fail(function(res) {
            console.log(res);
            $('#calendar-widget-container').text('Unable to load the calendar.');
        });
});
