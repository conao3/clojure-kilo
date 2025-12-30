# clojure-kilo

A minimal terminal text editor written in Clojure, inspired by [kilo](https://github.com/antirez/kilo).

## Overview

clojure-kilo is a lightweight, terminal-based text editor that demonstrates building a complete editor from scratch in Clojure. It features raw terminal mode handling, cursor navigation, scrolling, and a status bar.

## Features

- File viewing with syntax-aware tab rendering
- Full cursor navigation (arrow keys, Home, End, Page Up/Down)
- Horizontal and vertical scrolling
- Status bar with file information and cursor position
- Cross-line cursor movement

## Requirements

- Java 11 or later
- [Clojure CLI](https://clojure.org/guides/install_clojure) 1.11+

## Installation

```bash
git clone https://github.com/conao3/clojure-kilo.git
cd clojure-kilo
```

## Usage

### Running with Clojure CLI

```bash
clojure -M -m kilo.core [filename]
```

### Building and Running as Uberjar

```bash
clojure -T:build uberjar
java -jar target/kilo-standalone.jar [filename]
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| Arrow keys | Move cursor |
| Home | Go to beginning of line |
| End | Go to end of line |
| Page Up | Scroll up one screen |
| Page Down | Scroll down one screen |
| Ctrl+Q | Quit |

## License

This project is available as open source.
