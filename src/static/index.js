import "babel-polyfill";
import "./styles/app.css";
import "./styles/bulma.css";
import "font-awesome-webpack";

const Elm = require('../elm/Main');
// inject bundled Elm app into div#main
const app = Elm.Main.embed(document.getElementById('main'));
