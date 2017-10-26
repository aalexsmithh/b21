'use strict';

$(function() {
    $.get({
        url: '/api/get_calendar',
        dataType: 'json',
    })
    .done(function(res) {
        $('#calendar').clndr({
            events: _(res).map(function(event) {
                event.date = moment(event.date).local().startOf('day').format('YYYY-MM-DD');
            }),
            template: $('#calendar-template').html(),
        });
    })
    .fail(function(res) {
        console.log(res);
    });
});
