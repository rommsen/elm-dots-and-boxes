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

export const games = {
    start: game => {
        return database
            .ref(GAMES_REFPATH)
            .push(game);
    },

    // update: member => {
    //     return database
    //         .ref(GAMES_REFPATH + "/" + member.id)
    //         .set(member);
    // },

    ref: database.ref(GAMES_REFPATH)
}
