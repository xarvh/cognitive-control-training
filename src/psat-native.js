function registerElmPsatPorts(elmApp) {
    elmApp.ports.requestPq.subscribe(function () {

        // TODO: receive list of callable numbers and relative sounds
        var list = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        var randomIndex = Math.floor(Math.random() * list.length);
        var randomDigit = list[randomIndex];

        //TODO: play relative sound
        console.log('---->', randomDigit);

        elmApp.ports.newPq.send(randomDigit);
    });
}
