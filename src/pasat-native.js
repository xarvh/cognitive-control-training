function registerElmPorts(elmApp) {


    var sounds = {};
    elmApp.ports.loadSoundsPort.subscribe(function (soundNames) {
        loaded = 0;

        soundNames.forEach(function (soundName) {
            sounds[soundName] = new Audio('assets/sounds/' + soundName + '.ogg');
            sounds[soundName].preload = 'auto';
            sounds[soundName].oncanplaythrough = function () {
                loaded += 1;
                elmApp.ports.loadSoundsProgressPort.send(loaded / soundNames.length);
            };
        });
    });

    elmApp.ports.playSoundPort.subscribe(function (soundName) {
        if (!sounds[soundName]) throw new Error(soundName + ' not loaded');
        sounds[soundName].play();
    });


    elmApp.ports.downloadPort.subscribe(function (tuple) {
        var name = tuple[0];
        var type = tuple[1];
        var contents = tuple[2];
        saveAs(new Blob([contents], { type: type }), name);
    });
}
