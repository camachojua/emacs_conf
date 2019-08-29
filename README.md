# emacs_conf This is my personal `init.el` configuration file for
emacs. Just clone it and copy the file in your `~/.emacs.d/` directory
and launch emacs, then execute the following commands:

+ `M-x pdf-tools-install`
+ `M-x all-the-icons-install-fonts`

## Dependencies

The configuration uses `use-package`as a utility to speed up the
startup time while providing a flexible configuration. To facilitate
the use of emacs as a programming and text editing environment the
following dependencies are installed automagically:

+ `whitespace`
+ `autopair`
+ `rainbow-mode`
+ `rainbow-delimiters`
+ `all-the-icons`
+ `doom-modeline`
+ `dashboard`
+ `pdf-tools`
+ `nov`
+ `ivy`
+ `ivy-rich`
+ `swiper`
+ `projectile`
+ `company`
+ `yasnippet`
+ `magit`
+ `web-mode`
+ `php-mode`
+ `auctex`
+ `company-php`
+ `tex`
+ `nasm-mode`
+ `helm`
+ `smtpmail`
+ `request`
+ `dockerfile-mode`
+ `yaml-mode`
+ `json-mode`
+ `yaml-mode`
+ And much more..

## External dependencies

In order to provide a true `emacs` experience, this configuration
relies on external programs:

+ A `texlive` distribution for LaTeX code.
+ The `mu` program for displaying e-mails.
+ `offlineimap` for fetching your e-mails.
+ `git` for code versioning.

Please refer to the `mu`
[user manual](http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html#Gmail-configuration) for installation and configuration.

The `texlive` distribution comes with `tlmgr` to help the user with
the installation of new LaTeX packages.

## Screenshot

![screenshot](https://raw.githubusercontent.com/camachojua/emacs_conf/master/screenshot.png)
