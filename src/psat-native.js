function registerElmPsatPorts(elmApp) {

    // TODO: should not be using a different list.
    // Hopefully a we'll be using a better construct once multi language support is in place.
    var sounds = [1, 2, 3, 4, 5, 6, 7, 8, 9].map(function (n) {
        var a = new Audio('tasks/numbers/sounds/' + 'english/ossi' + '/' + n + '.ogg');
        a.preload = 'auto';
        return a;
    });

    elmApp.ports.playSoundPort.subscribe(function (pq) {
        sounds[pq - 1].play();
    });
}
