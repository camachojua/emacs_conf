# emacs_conf This is my personal `init.el` configuration file for
emacs. Just clone it and copy the file in your `~/.emacs.d/` directory
and launch emacs, then execute the following commands:

+ `M-x pdf-tools-install`
+ `M-x all-the-icons-install-fonts`

Now you need to copy the `systemd` service file to your `.config` directory and enable the service:

```bash
cp emacs.service ~/.config/systemd/user/
sudo systemctl enable --user emacs
sudo systemctl start --user emacs
```

## Dependencies

- =emacs 27=

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
+ `gnutls-bin` for slack communication.

Please refer to the `mu`
[user manual](http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html#Gmail-configuration) for installation and configuration.

The `texlive` distribution comes with `tlmgr` to help the user with
the installation of new LaTeX packages.

## Python development

Python projects that contain a `Pipfile` inside a git repository are
handled automatically.  `pipenv-mode` is enabled and the virtual
environment is activated as soon as the project is opened.  A Python
REPL is launched with `run-python` using the project's environment and
`dap-mode` provides debugging support via `debugpy`.

## Screenshots

![screenshot](https://raw.githubusercontent.com/camachojua/emacs_conf/master/screenshot.png)

![screenshot2](./screenshot2.png)
