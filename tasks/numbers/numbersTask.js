'use strict';

/*
 * TODO
 *
 * - decouple event storage and CSV export (use an object to describe an event)
 */

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
    assert(typeof options.formatTimestamp === 'function');

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
    var events = [['Timestamp', 'ISI (ms)', 'Event']];

    function pushEvent(name) {
        events.push([options.formatTimestamp(new Date), isi, name]);
    }

    function isSuccess(e) { return e[2] === RIGHT; }
    function isFailure(e) { return e[2] === WRONG || e[2] === MISS; }


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


    function getAggregateEvents() {
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
            var sessionStart = _.first(evs)[0];
            var sessionEnd = _.last(evs)[0];

            var counts = _.countBy(evs, (e) => e[2]);
            var right = counts[RIGHT] || 0;
            var wrong = counts[WRONG] || 0;
            var miss = counts[MISS] || 0;

            var accuracy = right / (right + wrong + miss);

            var startingIsi = _.first(evs)[1];
            var maxIsi = _(evs).map((e) => e[1]).max();
            var minIsi = _(evs).map((e) => e[1]).min();
            var by = _(evs).filter((e) => e[1] === minIsi).countBy((e) => e[2]).value();
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

        var evs = events.slice(1);
        var lastStart = 0;
        evs.forEach(function (e, index) {
            if (e[2] !== STOP) return;

            var sessionEvents = evs.slice(lastStart, index + 1);
            assert(sessionEvents[0][2] === START, sessionEvents[0][2]);
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
    this.getEvents = () => events;
    this.getAggregateEvents = getAggregateEvents;
}
