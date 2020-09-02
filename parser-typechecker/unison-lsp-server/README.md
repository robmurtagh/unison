# unison-lsp-server

A server implementation of the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

## 🚀 Quickstart

## Develop

[ghcid](https://github.com/ndmitchell/ghcid) is a terminal-based tool for quickly telling you compiler errors in your code:

```sh
# from repo root directory
stack install ghcid
ghcid "--command=stack ghci parser-typechecker/unison-lsp-server/Main.hs"
```

## Build

```sh
stack build unison-parser-typechecker:unison-lsp-server
```

## Install

You should now be able to install the executable, and use it as a backend for the language server client:

```sh
stack install unison-parser-typechecker:unison-lsp-server
# this should tell you to where it is installed, typically ~/.local/bin
find ~/.local/bin/ -name unison-lsp-server
```
