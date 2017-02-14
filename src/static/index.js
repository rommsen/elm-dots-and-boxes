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
            console.error("startGame:", err);
        });
});

app.ports.changeGame.subscribe(game => {
    games.update(game)
        .catch(err => {
            console.error("changeGame:", err);
        });
});

app.ports.registerLocalPlayer.subscribe(player => {
    players.register(player)
        .then(function(val) {
            app.ports.localPlayerRegistered.send({
                id: val.key,
                name: player.name
            });
        })
        .catch(err => {
            console.error("startGame:", err);
        });
});

app.ports.requestToJoinGame.subscribe(request => {
    games.requestToJoinGame(request)
        .catch(err => {
            console.error("requestToJoinGame:", err);
        });
});


games.ref.orderByChild("status").equalTo("Open").on("child_added", data => {
    const game = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.openGameAdded.send(game);
});

games.ref.orderByChild("status").equalTo("Open").on("child_removed", data => {
    app.ports.openGameRemoved.send(data.key);
});

// games.ref.orderByChild("status").equalTo("Running").on("child_added", data => {
//     const game = Object.assign({}, data.val(), {
//         id: data.key
//     });
//     app.ports.runningGameAdded.send(game);
// });
//
// games.ref.orderByChild("status").equalTo("Running").on("child_removed", data => {
//     app.ports.runningGameRemoved.send(data.key);
// });

games.ref.on("child_changed", data => {
    const game = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.gameChanged.send(game);
});
