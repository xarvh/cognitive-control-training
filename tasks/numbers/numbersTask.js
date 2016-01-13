'use strict';


var assert;
if (typeof assert === 'undefined') {
    assert = function assert(thing, message) {
        if (!thing) throw new Error(message || 'Assertion Error');
    }
}


function NumbersTask(options) {
    assert(options);
    assert(typeof options.duration === 'number');
    assert(typeof options.tellNumber === 'function');
    assert(typeof options.onCount === 'function');


    const NUMBERS = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    const RIGHT = 'right';
    const WRONG = 'wrong';
    const MISS = 'miss';

    // Task state
    var nextNumberTimeoutId;
    var currentNumber;
    var previousNumber;
    var userHasAnswered;
    var inverseSpeed;

    // Events
    var events = [['Browser Time (utc)', 'Inverse Speed (ms)', 'Event']];

    function pushEvent(name) {
        events.push([new Date().toISOString(), inverseSpeed, name]);
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
        pushEvent('start');
    }


    function stop() {
        if (!nextNumberTimeoutId) return;
        clearTimeout(nextNumberTimeoutId);
        nextNumberTimeoutId = null;
        pushEvent('stop');
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

        if (events.slice(-5).every(isFailure)) {
            inverseSpeed += 100;
            pushEvent('slower');
        }

        else if (events.slice(-5).every(isSuccess)) {
            inverseSpeed -= 100;
            pushEvent('faster');
        }

        options.onCount(result, inverseSpeed);
    }


    this.NUMBERS = NUMBERS;
    this.RESULTS = [RIGHT, MISS, WRONG];
    this.start = start;
    this.stop = stop;
    this.setUserAnswer = setUserAnswer;
    this.getEvents = () => events;
}
