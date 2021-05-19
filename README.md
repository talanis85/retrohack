retrohack
=========

A libretro frontend. Currently only works with Linux.

Dependencies
------------

* libasound2
* libGL
* libGLU
* libglfw3
* libX11
* libXi
* libXrandr
* libXxf86vm
* libXcursor
* libXinerama
* libXext

Installation
------------

1. Install [stack](https://haskellstack.org)
2. `git clone https://github.com/talanis85/retrohack.git`
3. `cd retrohack && stack install`

Usage
-----

retrohack uses a REPL-style console interface. The following
commands are available:

* `loadcore /path/to/core.so`

Load a libretro core.

* `loadgame /path/to/rom`

Load a game ROM.

* `run`

Runs the game.

* `pause`

Pauses the game.

* `continue`

Continues a paused game.
