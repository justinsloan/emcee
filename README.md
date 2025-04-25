# Emcee
My personal Emacs config

## How to use it
	You can try Emcee without messing with your curent config. Simply clone this repository into your `emacs-home-directory`, `cd` into the `emcee` directory, and run Emacs from the terminal with `emacs -q --load emcee-init.el`.

	If you decide to use Emcee as your primary config, you can add the following to your `init.el` file.
	
`
(add-to-list 'load-path (locate-user-emacs-file "emcee/"))
(require 'emcee-init)
`

## Goal
Build a custom Emacs productivity workflow based on chord-less keybindings.

## Non-goals
 - Not an attempt to replace the standard Emacs user experience. All default keybindings still still work.
