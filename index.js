window.onload = function cortexModule() {


    var numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    var numberSounds = {};


    //
    // app entry point
    //
    loadSounds('male', function(progress) {
        console.debug(progress);
        if (progress >= 1) readyNewGame();
    });


    //
    //
    //
    var timeoutId = null;
    var speed = 2000;

    var currentNumber;
    var previousNumber;
    var userHasClickedNumber;


    function clockNumberButton(number) {
        if (userHasClickedNumber) return;
        if (!previousNumber) return;
        userHasClickedNumber = true;

        if (number == currentNumber + previousNumber) console.log('correct');
        else console.log('wrong');
    }




    function giveNumber() {
        if (!userHasClickedNumber && previousNumber) console.log('missed');
        userHasClickedNumber = false;
        previousNumber = currentNumber;
        currentNumber = numbers[Math.floor(Math.random() * numbers.length)];

        numberSounds[currentNumber].play();

        timeoutId = setTimeout(giveNumber, speed);
    }






    //
    // game status
    //
    var startStop = document.getElementById('startstop');

    function startGame() {
        startStop.innerText = 'Stop';
        startStop.onclick = stopGame;

        timeoutId = window.setTimeout(giveNumber, 1000);
    };

    function stopGame() {
        window.clearTimeout(timeoutId);
        currentNumber = previousNumber = null;
        userHasClickedNumber = null;
        startStop.innerText = 'Start';
        startStop.onclick = startGame;
    };

    function readyNewGame() {
        stopGame();
        // do other resetty stuff
    }


    //
    // load up
    //
    function loadSounds(voice, progress) {
        var loaded = 0;
        numbers.forEach(function (n) {
            numberSounds[n] = new Audio('numbers/' + voice + '/' + n + '.ogg');
            numberSounds[n].preload = 'auto';
            numberSounds[n].oncanplaythrough = function () {
                loaded += 1;
                progress(loaded / numbers.length);
                numberSounds[n].oncanplaythrough = function () {};
            };
        });
    }
};
