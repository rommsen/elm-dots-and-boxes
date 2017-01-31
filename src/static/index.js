import "babel-polyfill";
import "./styles/app.css";
import "./styles/bulma.css";
import "font-awesome-webpack";

import {
    games,
    players
} from "./firebase";

const Elm = require('../elm/Main');
const app = Elm.Main.embed(document.getElementById('main'));

let gameId;

app.ports.openGame.subscribe(game => {
    games.open(game)
        .then(function(val) {
            app.ports.gameOpened.send(val.key);
        })
        .catch(err => {
            console.error("startGame error:", err);
        });
});

app.ports.changeGame.subscribe(game => {
    console.log(game)
    games.update(game)
        .catch(err => {
            console.error("changeGame error:", err);
        });
});

app.ports.registerPlayer.subscribe(player => {
    players.register(player)
        .then(function(val) {
            app.ports.playerRegistered.send({
                id: val.key,
                name: player.name
            });
        })
        .catch(err => {
            console.error("startGame error:", err);
        });
});


games.ref.on("child_added", data => {
    const game = Object.assign({}, data.val(), {
        id: data.key
    });
    gameId = data.key;
    console.log(game);
    // app.ports.gameStarted.send(game);
});


games.ref.on("child_changed", data => {
    const game = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.gameChanged.send(game);
});
