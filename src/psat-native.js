function registerElmPsatPorts(elmApp) {

    // TODO: should not be using a different list.
    // Hopefully a we'll be using a better construct once multi language support is in place.
    var sounds = [1, 2, 3, 4, 5, 6, 7, 8, 9].map(function (n) {
        var a = new Audio('tasks/numbers/sounds/' + 'english/ossi' + '/' + n + '.ogg');
        a.preload = 'auto';
        return a;
    });

    elmApp.ports.requestPq.subscribe(function (availablePqs) {

        var randomIndex = Math.floor(Math.random() * availablePqs.length);
        var randomDigit = availablePqs[randomIndex];

        sounds[randomIndex].play();

        elmApp.ports.newPq.send(randomDigit);
    });
}
