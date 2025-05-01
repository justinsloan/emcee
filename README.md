# Emcee
My personal Emacs config

## How to use it
You can try Emcee without messing with your curent config. Simply clone this repository into your `emacs-home-directory`, `cd` into the `emcee` directory, and run Emacs from the terminal with `emacs -q --load emcee-init.el`.

If you decide to use Emcee as your primary config, you can add the following to your `init.el` file.
	
```
(add-to-list 'load-path (locate-user-emacs-file "emcee/"))
(require 'emcee-init)
```

## Goal
Build a custom Emacs productivity workflow based on chord-less keybindings.

## Feature Map
 - Open epub files in a dedicated vertical-split window
 - Not an attempt to replace the standard Emacs user experience. All default keybindings still still work.

## Features
 - Chord-less menu system based on Transient
 - "Actions"-based productivity workflow
 - Enhanced text navigation/editing
 - Clean auto-save experience
 - Simplified modeline
 - Fully self-installing and self-updating

## TODO
 - Check if Fira Mono is installed, default to another font if not found
 - Automatically check GitHub repo for updates
 - Write the manual
