# Emacs config — notes for Claude

## Environment
- GNU Emacs 30.2 (x86_64-w64-mingw32), native Windows build.
- This folder is `~/.emacs.d` (`C:\Users\k.c.juntunen\AppData\Roaming\.emacs.d`), a git repo.
- `at-work` (set in `early-init.el`) is true on the work machine, `pascal`.

## Layout
- `early-init.el` — defines `at-work`.
- `init.el` — loads `package-config.d/NN.*.el` in numeric order, then
  `keybindings.el`, `abbrevs.el`, `york-mode.el` (work only), `~/.personal`.
- `package-config.d/` — one file per package, named `NN.name-config.el`.
- `york-mode.el` — work-specific commands; workarea is `york-workarea`
  (`C:/fastrack/workarea/`).

## Conventions
- Every file starts with `-*- lexical-binding: t; -*-`.
- Indent with tabs, width 2 (see `.editorconfig`).
- Favor UNIX-style line endings.
- Prefixes: `kc/` for personal helpers, `york-` / `york--` for work code.
- Work keybindings live under `C-c y`. Check for collisions before adding one.

## Testing
- Byte-compile check: `emacs -Q --batch -f batch-byte-compile FILE.el`
- Full startup check: `emacs --debug-init`
- Clean up any `.elc` files a test leaves behind.

## Preferences
- Let me know if a task may take more than a couple of minutes.
- Suggest, but don't add extra hardening without asking.
- Don't commit unless I ask.
