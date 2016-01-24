'use strict';


if (typeof module === 'object')
    module.exports = NumbersTask;


var assert;
if (typeof assert === 'undefined') {
    assert = function assert(thing, message) {
        if (!thing) throw new Error(message || 'Assertion Error');
    }
}


function NumbersTask(options) {
    assert(options);
    assert(typeof options.emitDigit === 'function');
    assert(typeof options.onCount === 'function');

    const DIGITS = [1, 2, 3, 4, 5, 6, 7, 8, 9];

    const RIGHT = 'right';
    const WRONG = 'wrong';
    const MISS = 'miss';

    const START = 'start';
    const STOP = 'stop';

    // Task state
    var nextDigitTimeoutId;
    var currentDigit;
    var previousDigit;
    var userHasAnswered;
    var isi; // inter-stimulus interval, the time between emitDigit() calls

    // Events
    var events = [];

    function pushEvent(name) {
        events.push({ timestamp: new Date, isi: isi, name: name });
    }

    function isSuccess(e) { return e.name === RIGHT; }
    function isFailure(e) { return e.name === WRONG || e.name === MISS; }


    //
    function start(startingIsi, initialDelay) {
        assert(typeof startingIsi === 'number');
        assert(!nextDigitTimeoutId, 'numbersTask already running');

        isi = startingIsi;
        currentDigit = previousDigit = null;
        userHasAnswered = null;
        nextDigitTimeoutId = setTimeout(nextDigit, initialDelay || 500);
        pushEvent(START);
    }


    function stop() {
        if (!nextDigitTimeoutId) return;
        clearTimeout(nextDigitTimeoutId);
        nextDigitTimeoutId = null;
        pushEvent(STOP);
    }


    function nextDigit() {
        if (!userHasAnswered && previousDigit) count(MISS);

        userHasAnswered = false;
        previousDigit = currentDigit;
        currentDigit = DIGITS[Math.floor(Math.random() * DIGITS.length)];

        nextDigitTimeoutId = setTimeout(nextDigit, isi);
        options.emitDigit(currentDigit);
    }


    function setUserAnswer(number) {
        if (!previousDigit) return;
        if (userHasAnswered) return;
        userHasAnswered = true;
        count(number == currentDigit + previousDigit ? RIGHT : WRONG);
    }


    function count(result) {
        pushEvent(result);

        if (events.slice(-4).every(isFailure)) {
            isi += 100;
            pushEvent('slower');
        }

        else if (events.slice(-4).every(isSuccess)) {
            isi -= 100;
            pushEvent('faster');
        }

        options.onCount(result, isi);
    }


    function getEventsTable(formatTimestamp) {
         return [['Timestamp', 'ISI (ms)', 'Event']].concat(events.map((e) => [ formatTimestamp(e.timestamp), e.isi, e.name ]));
    }


    function getAggregateEventsTable(formatTimestamp) {
        var sessions = [[
            'Session start',
            'Session end',

            'Right',
            'Wrong',
            'Miss',

            'Accuracy (normalized)',

            'Starting ISI',
            'Max ISI',
            'Min ISI',
            'Trials at minimum ISI'
        ]];


        function addSession(evs) {
            var sessionStart = formatTimestamp(_.first(evs).timestamp);
            var sessionEnd = formatTimestamp(_.last(evs).timestamp);

            var counts = _.countBy(evs, 'name');
            var right = counts[RIGHT] || 0;
            var wrong = counts[WRONG] || 0;
            var miss = counts[MISS] || 0;

            var accuracy = right / (right + wrong + miss);

            var startingIsi = _.first(evs).isi;
            var maxIsi = _(evs).map('isi').max();
            var minIsi = _(evs).map('isi').min();
            var by = _(evs).filter({ isi: minIsi }).countBy('name').value();
            var trialsAtMinIsi = (by[RIGHT] || 0) + (by[WRONG] || 0) + (by[MISS] || 0);

            sessions.push([
                    sessionStart,
                    sessionEnd,
                    right,
                    wrong,
                    miss,
                    accuracy,
                    startingIsi,
                    maxIsi,
                    minIsi,
                    trialsAtMinIsi,
            ]);
        }

        var lastStart = 0;
        events.forEach(function (e, index) {
            if (e.name !== STOP) return;

            var sessionEvents = events.slice(lastStart, index + 1);
            assert(sessionEvents[0].name === START, sessionEvents[0].name);
            addSession(sessionEvents);
            lastStart = index + 1;
        });

        return sessions;
    }


    this.DIGITS = DIGITS;
    this.RESULTS = [RIGHT, MISS, WRONG];
    this.start = start;
    this.stop = stop;
    this.setUserAnswer = setUserAnswer;
    this.getEvents = () => events.slice();
    this.getEventsTable = getEventsTable;
    this.getAggregateEventsTable = getAggregateEventsTable;
}
