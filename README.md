
# TEA-Chess

## A chess-themed tutorial for writing an SPA in Bucklescript-TEA 

I recently started frontend web development in OCaml by using 
[Bucklescript](https://bucklescript.github.io/) to compile my code into Javascript.  Naturally, a functional
language like OCaml goes well together with the famous [Elm
architecture](https://guide.elm-lang.org/architecture/), so I chose to explore the recent [Bucklescript-TEA](https://github.com/OvermindDL1/bucklescript-tea)
library.  (TEA stands for "The Elm Architecture", obviously.)

OCaml and Bucklescript have recently gained a lot of attention, not
least because of Facebook's [Reason](https://reasonml.github.io/) &#x2013; an alternative, more
Javascript-like syntax for OCaml. I prefer the traditional OCaml
syntax, but you can use either syntax together with Bucklescript.

Since I was absolutely not interested in implementing yet another [Todo
app](http://todomvc.com/), I decided to create an interactive chess board with move
validation (supplied by an external library), drag and drop, move
logging as well as take-backs.  I also implemented
a game tree representing alternative moves, by
using a functional programming technique called the ["zipper"](https://pavpanchekha.com/blog/zippers/huet.html).
Finally, I added a REST backend (also in OCaml) for the frontend to
talk to.

In this ["Twelve Days of Christmas"](https://en.wikipedia.org/wiki/Twelve_Days_of_Christmas) tutorial, I will walk you through the entire example, with
version control snapshots along the way. It will certainly help if you
have a little experience with chess, web development and functional programming in
general or OCaml in particular, but it
is no prerequisite, you can pick up everything you need on the way.

## Table of contents 
* [Day 1 (Dec 25, 2018)](doc/day1/index.org)
* [Day 2 (Dec 26, 2018)](doc/day2/index.org)
* [Day 3 (Dec 27, 2018)](doc/day3/index.org)
