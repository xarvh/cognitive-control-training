'use strict';


window.onload = function () {

    var numberSounds = {};

    var game = new NumbersTask({
        tellNumber: (number) => numberSounds[number].play(),
        onCount: onCount,
        formatTimestamp: (d) => moment(d).format('YYYY-MM-DD HH:mm:ss.SSS'),
    });


    //
    // app entry point
    //
    addButtons();
    loadSounds('english/ossi', function (progress) {
        if (progress >= 1) readyNewGame();
    });


    function addButtons() {
        var container = document.getElementById('number-buttons-container');

        var buttons = {};
        game.NUMBERS.forEach((n) => game.NUMBERS.forEach((m) => buttons[n + m] = true));
        var N = Object.keys(buttons).map(Number).sort((a, b) => a - b);

        N.forEach(function (n, index) {
            var button = document.createElement('div');
            button.className = 'number-button';
            button.innerHTML = n.toString();
            button.onclick = () => game.setUserAnswer(n);

            var angle = Math.PI * 2 * ((index + 1) / (N.length + 1));

            var top = container.clientHeight/2.5 * (1 - Math.cos(angle));
            var left = container.clientWidth/2.5 * (1 + Math.sin(angle));

            button.style.top = top + 'px';
            button.style.left = left + 'px';

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
    var sessionTimestamp = new Date;
    function downloadCsv(dataArrayOfArrays, name) {
        var contents = dataArrayOfArrays.map((e) => e.map((v) => '"' + v + '"').join(',') + '\n').join('');
        var blob = new Blob([contents], { type: 'text/csv' });
        saveAs(blob, 'numbersTask_' + moment(sessionTimestamp).format('YYYYMMDD_HHmm') + '_' + name + '.csv');
    }

    document.getElementById('download-log').onclick = () => downloadCsv(game.getEvents(), 'log');
    document.getElementById('download-aggregate').onclick = () => downloadCsv(game.getAggregateEvents(), 'aggregate');


    //
    // game status
    //
    var startStop = document.getElementById('startstop');
    var durationTimeoutId = null;


    function startGame() {
        startStop.innerHTML = 'Stop';
        startStop.onclick = stopGame;

        game.start(+document.getElementById('speed').value);
        var duration = +document.getElementById('duration').value;
        if (duration) durationTimeoutId = setTimeout(stopGame, duration * 60 * 1000);
    }


    function stopGame() {
        clearTimeout(durationTimeoutId);
        game.stop();
        startStop.innerHTML = 'Start';
        startStop.onclick = startGame;
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
