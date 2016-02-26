## Emacs Wybe Major Mode

wybe-mode is a major mode for editing Wybe source code in GNU Emacs.

## Installation

Place `wybe-mode.el` somewhere in your Emacs load-path and add the following
lines to your Emacs init file to associate `wybe-mode` with `.wybe` files.

    (add-to-list 'auto-mode-alist '("\\.wybe\\'" . wybe-mode))
    
