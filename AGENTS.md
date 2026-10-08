# Skyline-Tool — Agent Guidelines

## Overview

Skyline-Tool is a Common Lisp toolchain for compiling graphics, maps, music, and other assets into 8-bit and 16-bit formats for retro game platforms. It is the asset pipeline behind Phantasia.

## Key Paths

| Item | Path |
|---|---|
| System definition | `skyline-tool.asd` |
| Package / exports | `src/package.lisp` |
| Command entry point | `src/interface.lisp` |
| Asset allocation | `src/asset-allocator.lisp` |
| Graphics | `src/graphics.lisp` |
| Maps | `src/maps.lisp` |
| Music | `src/music.lisp` |
| OOP support | `src/oops.lisp` |
| 7800 GD interface | `src/7800gd-interface.lisp` |
| Tests | `tests/` (FiveAM suites) |

## Running

In Slime REPL:

```
,cd ~/Projects/Phantasia/SkylineTool
(asdf:load-asd "skyline-tool.asd")
(ql:quickload :skyline-tool)
```

Make echoes each invocation as `(Skyline-Tool:Command ...)` — paste into Slime to re-run interactively. See `Skyline-Tool:Command` in `src/interface.lisp`.

## Lisp Style

- Format in Emacs with Slime. Conform to project conventions.
- Public API functions (exported from `package.lisp`) should have docstrings.

## Testing

```
(asdf:test-system :skyline-tool/test)
```

Runs all FiveAM suites. Run a single suite:

```
(fiveam:run! :suite-name)
```

## Dependencies

- **`:eightbol`** (in `eightbol/`) — COBOL-to-assembly OOP compiler.
- Other key deps: `:mcclim`, `:fiveam`, `:cl-ppcre`, `:cl-json`, `:png-read`, `:zip`, `:midi`, `:ironclad`.

## Phantasia Integration

- Phantasia's Makefile invokes `Skyline-Tool:Command` for asset compilation.
- `project-root` points to `../` (the Phantasia tree).
- `*machine*`, `*project.json*`, `*sound*`, etc. control per-platform dispatch.

## Changes Require Human Review

Any changes to Skyline-Tool source must be reviewed by a human. Push to a branch and contact the maintainer. Do not merge Skyline-Tool changes into the Phantasia PR until the Skyline-Tool PR is open.

## Cross-References

- **Eightbol:** `eightbol/AGENTS.md`
- **Phantasia:** `../AGENTS.md`
