;;; src/version-control/README.md

# Skyline-Tool Version Control Integration

This package provides version control integration for Skyline-Tool via Git/SVN backends and integrated issue tracking. Key components:

## Git Backend
- Full Git protocol implementation (init, clone, commit, push/pull, branch)
- Diff/difftool with Git CLI integration
- File status indicators (staged/modified/untracked/absent)

## Issue Tracking
- Embedded issue list with branch linking
- GitHub/GitLab/Bugzilla API clients
- Resource status display integration

## GUI Integration
- CLIM-based windows with Skyline-Tool-compatible titles
- Status indicators using Unicode symbols
- Resource presentation in reference/detail/editing views

## Configuration
- Centralized config in ~/.config/Skyline-Tool/<Title-Case Game-Title>/<machine-dir>-config.lisp

## License
MIT