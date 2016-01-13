'use strict'


function pad(n) {
    n = n.toString();
    while (n.length < 2) n = '0' + n;
    return n;
}


window.onload = function cortexModule() {

    var numberSounds = {};

    var game = new NumbersTask({
        duration: 5 * 60 * 1000,
        tellNumber: (number) => numberSounds[number].play(),
        onCount: onCount,
    });


    //
    // app entry point
    //
    addButtons();
    addDownload();
    loadSounds('english/ossi', function (progress) {
        if (progress >= 1) readyNewGame();
    });


    function addButtons() {
        var container = document.getElementById('buttons');

        var buttons = {};
        game.NUMBERS.forEach((n) => game.NUMBERS.forEach((m) => buttons[n + m] = true));
        var N = Object.keys(buttons).map(Number).sort((a, b) => a - b);

        N.forEach(function (n) {
            var button = document.createElement('button');
            button.innerHTML = n.toString();
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
            numberSounds[n] = new Audio('sounds/' + voice + '/' + n + '.ogg');
            numberSounds[n].preload = 'auto';
            numberSounds[n].oncanplaythrough = function () {
                loaded += 1;
                progress(loaded / game.NUMBERS.length);
                numberSounds[n].oncanplaythrough = function () {};
            };
        });
    }


    //
    // download
    //
    function addDownload() {
        var sessionStart = new Date();

        var filename = [
            'numbersTask_s',
            sessionStart.getFullYear(), pad(sessionStart.getMonth() + 1), pad(sessionStart.getDate()),
            '_',
            pad(sessionStart.getHours()),
            pad(sessionStart.getMinutes()),
            '.csv'
        ].join('');

        document.getElementById('download').onclick = function () {
            var contents = game.getEvents().map((e) => e.map((v) => '"' + v + '"').join(',') + '\n').join('');
            var blob = new Blob([contents], { type: 'text/csv;charset=utf-8' });
            saveAs(blob, filename);
        };
    }


    //
    // game status
    //
    var startStop = document.getElementById('startstop');


    function startGame() {
        startStop.innerHTML = 'Stop';
        startStop.onclick = stopGame;
        game.start(+document.getElementById('speed').value);
    }


    function stopGame() {
        startStop.innerHTML = 'Start';
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
        document.getElementById(result).innerHTML = results[result];
        document.getElementById('speed').value = speed;
    }
};
