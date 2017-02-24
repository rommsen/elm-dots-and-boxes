'use strict';

const config = {
    apiKey: "AIzaSyC281P_iZqMrZNY8W3qOrzQziXrWq9K-qM",
    authDomain: "elm-dots-and-boxes.firebaseapp.com",
    databaseURL: "https://elm-dots-and-boxes.firebaseio.com",
    storageBucket: "elm-dots-and-boxes.appspot.com",
    messagingSenderId: "712806609968"
};

const app = firebase.initializeApp(config);
const database = app.database();
const GAMES_REFPATH = "games";
const PLAYER_REFPATH = "players";
const CHAT_REFPATH = "chat";

export const games = {
    open: game => {
        return database
            .ref(GAMES_REFPATH)
            .push(game);
    },

    update: game => {
        return database
            .ref(`${GAMES_REFPATH}/${game.id}`)
            .set(game);
    },

    requestToJoinGame: request => {
        return database
            .ref(`${GAMES_REFPATH}/${request.gameId}/joinRequests`)
            .push(request.player);
    },

    watchGame: request => {
        return database
            .ref(`${GAMES_REFPATH}/${request.gameId}/spectators`)
            .push(request.player);
    },

    abandonOnDisconnect: gameId => {
        const ref = database
            .ref(`${GAMES_REFPATH}/${gameId}/status`);
        return ref.onDisconnect().set("Abandoned");
    },

    cancelAbandonOnDisconnect: gameId => {
        const ref = database
            .ref(`${GAMES_REFPATH}/${gameId}/status`);
        return ref.onDisconnect().cancel();
    },

    ref: database.ref(GAMES_REFPATH)
}

export const players = {
    register: player => {
        return database
            .ref(PLAYER_REFPATH)
            .push(player);
    },

    deleteOnDisconnect: playerId => {
        const ref = database
            .ref(`${PLAYER_REFPATH}/${playerId}`);
        return ref.onDisconnect().remove();
    },

    ref: database.ref(PLAYER_REFPATH)
}

export const chat = {
    send: msg => {
        return database
            .ref(CHAT_REFPATH)
            .push(msg);
    },

    ref: database.ref(CHAT_REFPATH)
}
