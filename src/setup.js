var elmApp = Elm.Main.fullscreen();

elmApp.ports.downloadPort.subscribe(function (tuple) {
    var name = tuple[0];
    var type = tuple[1];
    var contents = tuple[2];
    saveAs(new Blob([contents], { type: type }), name);
});
