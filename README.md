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

Command line
------------

Examples:

* Just load a core with a game:
  `retrohack /path/to/core.so /path/to/rom`

* Also load a lua script:
  `retrohack /path/to/core.so /path/to/rom script /path/to/script.lua`

* Also execute some lua commands at initialization:
  `retrohack /path/to/core.so /path/to/rom script /path/to/script.lua "some.global = true"`

Whatever you choose, you will still have to type `run` to start the emulator.

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

* `info`

  Shows information about the loaded core.

* `avinfo`

  More information about the loaded core :)

* `exec /path/to/script`

  Executes the given file line by line as if they were entered
  in the REPL.

* `peek <type> <segment> <address>`

  Get the value at the given address.
  `<type>` is one of:

  `i8`, `i16`, `i32` (signed integers),
  `u8`, `u16`, `u32` (unsigned integers)

  `<segment>` is one of:

  `sram`, `rtc`, `main`, `video`, `rom`

  (`rom` is not actually supported, but it works with a patched version
  of snes9x)

* `poke <type> <segment> <address> <value>`

  Set the value at the given address. See `peek` for arguments.
