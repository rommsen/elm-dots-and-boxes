# Elm Dots and Boxes
I build this project as a learning project to learn Elm.
I am more than happy for any reviews or ideas where and how to improve.

## What is it doing?
It is an online multiplayer implementation of [Dots and Boxes](https://en.wikipedia.org/wiki/Dots_and_Boxes).

## Why do I think it is cool?
* Multiplayer: up to five players!
* Spectator-Mode: watch games of other players
* many different board sizes
* turn timer to keep the game going

## Thats cool, can I play it online?
Yes please: https://elm-dots-and-boxes.firebaseapp.com/

## How to play?
* enter your name
* open a new game (choose box size or turn timer) or
* join an open game and wait for acceptance or
* watch an open or running game
* when opened: wait for other players and accept them (up to five) then
* start a new game
* when joined: wait for acceptance, when not accepted and game started you can watch the game
* when the owner of the game abandons the game, the other players are informed and can get back to the lobby
* play

## What is it based on?
* Elm (obviously)
* some webpack/babel for transpilation of ES6 and the dev-server / hot-reloading
* [Bulma](http://bulma.io/) for the CSS/Flexbox magic
* https://github.com/moarwick/elm-webpack-starter
* Firebase for the backend
* because I am shit in CSS, I took the basic idea (table as board game, divs that are pushed onto the tables boarder as edges) and the basic CSS from http://en.dots-game.org/


## Nice what else did you do in elm?
* [elm-giphy](https://github.com/rommsen/elm-giphy)
* [elm-bookkeping](https://github.com/rommsen/elm-bookkeeping)


## Getting started

### Installation
You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

If you have yarn installed (you should) you can just run

    yarn run installation


**Otherwise do the following:**

Install JS dependencies with:

    [yarn|npm] install

Install Elm dependencies with:

    elm package install


### Running
Start webpack-dev-server (recompiles when files change)

    [yarn|npm] run start   

Then view it:

    localhost:8080
