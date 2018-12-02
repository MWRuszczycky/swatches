# Swatches

## Overview

*Swatches* is a program for looking at all the colors your terminal can display in a variety of different formats. *Swatches* is written entirely in Haskell using [Brick](https://hackage.haskell.org/package/brick), and should run in both Linux and MacOS terminals, but probably not Windows (I've only tested it in Linux). Currently, you can do the following with *Swatches*:
1. Sort colors according to RGB or HSV values in sequential or blocked displays (see the `--sort` option).
2. View color values separated into greyscale, the base 16-palette and a 6x6x6 color cube that you can manipulate in *three* dimensions (see the `cube` mode).
3. Enter a hexcode to find the nearest matches based on the default hexcodes (see the `match` mode).

## Compiling and installation

*Swatches* uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). Clone the repository and build with
```sh
git clone https://github.com/MWRuszczycky/swatches.git
cd swatches
stack build
```
You can then run *Swatches* isolated within the repository using
```sh
stack exec swatches
```
Alternatively, you can install swatches locally using
```sh
stack install
```
instead of `stack build` from within the repository. *Stack* will tell you where the binary has been locally installed (e.g., `~/.local/bin`) in case you want to later uninstall *Swatches*. Now you can run the program anywhere by just typing `swatches` at the terminal.

## Terminal settings

By default *Swatches* sets the `TERM` environment parameter to `xterm-256color`. You can change this by using the `--terminal=MYTERM` option.

## Getting help

Swatches comes equipped with extensive help that details all of its features and usage. You can display help when running with *Stack* using
```sh
stack exec swatches -- --help | less
```
or if you have installed locally, try
```sh
swatches --help | less
```

## To do

1. Write a better README with some images.
2. Some functions still need commenting.
