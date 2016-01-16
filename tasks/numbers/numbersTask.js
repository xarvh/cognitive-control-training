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
    assert(typeof options.tellNumber === 'function');
    assert(typeof options.onCount === 'function');
    assert(typeof options.formatTimestamp === 'function');

    const NUMBERS = [1, 2, 3, 4, 5, 6, 7, 8, 9];

    const RIGHT = 'right';
    const WRONG = 'wrong';
    const MISS = 'miss';

    const START = 'start';
    const STOP = 'stop';

    // Task state
    var nextNumberTimeoutId;
    var currentNumber;
    var previousNumber;
    var userHasAnswered;
    var inverseSpeed;

    // Events
    var events = [['Timestamp', 'Inverse speed (ms)', 'Event']];

    function pushEvent(name) {
        events.push([options.formatTimestamp(new Date), inverseSpeed, name]);
    }

    function isSuccess(e) { return e[2] === RIGHT; }
    function isFailure(e) { return e[2] === WRONG || e[2] === MISS; }


    //
    function start(startingInverseSpeed) {
        assert(typeof startingInverseSpeed === 'number');

        inverseSpeed = startingInverseSpeed;
        currentNumber = previousNumber = null;
        userHasAnswered = null;
        nextNumberTimeoutId = nextNumberTimeoutId || setTimeout(nextNumber, 1000);
        pushEvent(START);
    }


    function stop() {
        if (!nextNumberTimeoutId) return;
        clearTimeout(nextNumberTimeoutId);
        nextNumberTimeoutId = null;
        pushEvent(STOP);
    }


    function nextNumber() {
        if (!userHasAnswered && previousNumber) count(MISS);

        userHasAnswered = false;
        previousNumber = currentNumber;
        currentNumber = NUMBERS[Math.floor(Math.random() * NUMBERS.length)];

        options.tellNumber(currentNumber);
        nextNumberTimeoutId = setTimeout(nextNumber, inverseSpeed);
    }


    function setUserAnswer(number) {
        if (!previousNumber) return;
        if (userHasAnswered) return;
        userHasAnswered = true;
        count(number == currentNumber + previousNumber ? RIGHT : WRONG);
    }


    function count(result) {
        pushEvent(result);

        if (events.slice(-4).every(isFailure)) {
            inverseSpeed += 100;
            pushEvent('slower');
        }

        else if (events.slice(-4).every(isSuccess)) {
            inverseSpeed -= 100;
            pushEvent('faster');
        }

        options.onCount(result, inverseSpeed);
    }


    function getAggregateEvents() {
        var sessions = [[
            'Session start',
            'Session end',

            'Right',
            'Wrong',
            'Miss',

            'Accuracy (normalized)',

            'Starting inverse speed',
            'Max inverse speed',
            'Min inverse speed',
            'Trials at min inverse speed'
        ]];


        function addSession(evs) {
            var sessionStart = _.first(evs)[0];
            var sessionEnd = _.last(evs)[0];

            var counts = _.countBy(evs, (e) => e[2]);
            var right = counts[RIGHT] || 0;
            var wrong = counts[WRONG] || 0;
            var miss = counts[MISS] || 0;

            var accuracy = right / (right + wrong + miss);

            var startingInverseSpeed = _.first(evs)[1];
            var maxInverseSpeed = _(evs).map((e) => e[1]).max();
            var minInverseSpeed = _(evs).map((e) => e[1]).min();
            var by = _(evs).filter((e) => e[1] === minInverseSpeed).countBy((e) => e[2]).value();
            var trialsAtMinInverseSpeed = (by[RIGHT] || 0) + (by[WRONG] || 0) + (by[MISS] || 0);

            sessions.push([
                    sessionStart,
                    sessionEnd,
                    right,
                    wrong,
                    miss,
                    accuracy,
                    startingInverseSpeed,
                    maxInverseSpeed,
                    minInverseSpeed,
                    trialsAtMinInverseSpeed,
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


    this.NUMBERS = NUMBERS;
    this.RESULTS = [RIGHT, MISS, WRONG];
    this.start = start;
    this.stop = stop;
    this.setUserAnswer = setUserAnswer;
    this.getEvents = () => events;
    this.getAggregateEvents = getAggregateEvents;
}
