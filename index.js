'use strict'


var assert;
if (typeof assert === 'undefined') {
    assert = function assert(thing, message) {
        if (!thing) throw new Error(message || 'Assertion Error');
    }
}


function Game(options) {
    assert(options);
    assert(typeof options.duration === 'number');
    assert(typeof options.tellNumber === 'function');
    assert(typeof options.onCount === 'function');


    const NUMBERS = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    const RIGHT = 'right';
    const WRONG = 'wrong';
    const MISS = 'miss';


    var inverseSpeed;
    var events = [];


    var nextNumberTimeoutId;
    var currentNumber;
    var previousNumber;
    var userHasAnswered;


    function start(startingInverseSpeed) {
        assert(typeof startingInverseSpeed === 'number');

        inverseSpeed = startingInverseSpeed;
        currentNumber = previousNumber = null;
        userHasAnswered = null;
        nextNumberTimeoutId = nextNumberTimeoutId || setTimeout(nextNumber, 1000);
        events.push(inverseSpeed);
    }


    function stop() {
        clearTimeout(nextNumberTimeoutId);
        nextNumberTimeoutId = null;
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
        events.push(result);

        if (events.slice(-5).every((r) => r === WRONG || r === MISS)) {
            inverseSpeed += 100;
            events.push(inverseSpeed);
        }

        else if (events.slice(-5).every((r) => r === RIGHT)) {
            inverseSpeed -= 100;
            events.push(inverseSpeed);
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




window.onload = function cortexModule() {

    var numberSounds = {};

    var game = new Game({
        duration: 5 * 60 * 1000,
        tellNumber: (number) => numberSounds[number].play(),
        onCount: onCount,
    });


    //
    // app entry point
    //
    addButtons();
    loadSounds('english/male', function (progress) {
        if (progress >= 1) readyNewGame();
    });


    function addButtons() {
        var container = document.getElementById('buttons');

        var buttons = {};
        game.NUMBERS.forEach((n) => game.NUMBERS.forEach((m) => buttons[n + m] = true));
        var N = Object.keys(buttons).map(Number).sort((a, b) => a - b);

        N.forEach(function (n) {
            var button = document.createElement('button');
            button.innerText = n;
            button.onclick = () => game.setUserAnswer(n);

            var angle = Math.PI * 2 * ((n - N[0] + 0.5) / N.length);

            var top = container.clientHeight/2.5 * (1 - Math.cos(angle));
            var left = container.clientWidth/2.5 * (1 + Math.sin(angle));

            button.style.position = 'absolute';
            button.style.top = top + 'px';
            button.style.left = left + 'px';
            button.style.width = '30px';
            button.style.height = '20px';

            container.appendChild(button);
        });
    }


    function loadSounds(voice, progress) {
        var loaded = 0;
        game.NUMBERS.forEach(function (n) {
            numberSounds[n] = new Audio('numbers/' + voice + '/' + n + '.ogg');
            numberSounds[n].preload = 'auto';
            numberSounds[n].oncanplaythrough = function () {
                loaded += 1;
                progress(loaded / game.NUMBERS.length);
                numberSounds[n].oncanplaythrough = function () {};
            };
        });
    }


    //
    // game status
    //
    var startStop = document.getElementById('startstop');


    function startGame() {
        startStop.innerText = 'Stop';
        startStop.onclick = stopGame;
        game.start(+document.getElementById('speed').value);
    }


    function stopGame() {
        startStop.innerText = 'Start';
        startStop.onclick = startGame;
        game.stop();
    }


    function readyNewGame() {
        stopGame();
        // do other resetty stuff
    }


    var results = {right: 0, wrong: 0, miss: 0};
    function onCount(result, speed) {
        results[result] += 1;
        console.log(result);
        document.getElementById(result).innerText = results[result];
        document.getElementById('speed').value = speed;
    }
};
