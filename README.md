# opam-minver

`opam-minver` finds the minimum dependency versions for an opam project by
directly testing them.

Given a project directory containing a `.opam` file, it reads the declared
dependencies, binary-searches each one's available versions to find the
oldest version that still builds and passes tests, and optionally writes the
discovered lower bounds back into the `.opam` file.

## Installation

```
opam install opam-minver
```

## Usage

Run from the project directory you want to analyze:

```
opam-minver
```

By default the tool runs in **dry-run mode**: it prints what it would write
without modifying anything. Pass `--write` (or `-w`) to update the `.opam`
file in place:

```
opam-minver --write
```

If the project is in a different directory, use `--dir`:

```
opam-minver --write --dir /path/to/my-package
```

Any existing dependency version bounds in the opam file will be used as limits
to the search space. This can save time if you know of any incompatibilities,
but it also means that if you don't, you should probably remove any bounds
you aren't certain of and let the tool discover them.

### Subcommands and options

```
opam-minver [OPTION]…
opam-minver delete [--dry-run]
```

| Option | Description |
|---|---|
| `--write`, `-w` | Write discovered bounds to the `.opam` file (default: dry-run) |
| `--dir DIR` | Project directory to analyse (default: current directory) |
| `--keep-switches` | Keep temporary opam switches after the run |
| `--keep-json` | Keep `opam-minver.json` after a successful `--write` run |
| `--log-file [FILE]` | Enable debug logging; generates a timestamped filename if `FILE` is omitted |
| `--quiet`, `-q` | Suppress per-probe progress output |

The `delete` subcommand removes all `opam-minver-` switches without running a
search. `--dry-run` shows what would be removed without actually deleting.

## How it works

1. **Parse** the `.opam` file to collect all declared dependencies.
2. **Probe the OCaml compiler** first. The current compiler is verified to
   build and test the project, then the available OCaml versions are
   binary-searched for the oldest passing version. OCaml 4 and OCaml 5
   are searched independently, since a package can have different minimum
   requirements for each major version. If an OCaml lower bound already
   exists in the `.opam` file, the bound version is probed first within each
   series; if it passes, the binary search for that series is skipped entirely.
3. **Probe each dependency** within a dedicated switch for each OCaml major
   version. All dependencies are tested in the same switch (with pins applied
   one at a time) rather than creating a new switch per version, which keeps
   the search fast. If a dependency already carries a lower bound in the
   `.opam` file, that bound version is probed first; if it passes, the binary
   search is skipped entirely, so runs over a well-bounded file are
   significantly faster.
4. **Run combined validations.** Once all per-dependency searches converge,
   the discovered minimums are pinned together in a fresh switch and the
   project is built and tested again. This catches any interactions between
   packages that the independent per-dep searches could not see. If the
   minimum version for any dependency differs between OCaml 4 and OCaml 5, an
   additional validation pins the higher minimums into the OCaml 4
   switch to verify they can be installed there: a warning is printed if they
   cannot.
5. **Write the results** back into the `.opam` file's `depends:` block once
   all searches have converged. Each dependency gets a single `>= version`
   constraint. If the minimum version differs between OCaml 4 and OCaml 5,
   the higher of the two is used and a note is printed, as opam's dependency
   filter model does not reliably support per-major-version bounds. Switches
   are removed unless `--keep-switches` was passed.

The binary search assumes that each dependency's compatibility is monotone:
if version N passes, all later versions also pass. This holds for the vast
majority of packages. Interdependencies between packages, where the minimum
version of one dep depends on the version of another, are not modelled; each
dep is searched independently. This keeps the search space small.

## Resumability

Progress is saved to `opam-minver.json` in the project directory after every
probe. If a run is interrupted, restarting `opam-minver` in the same directory
will pick up where it left off: already-known pass/fail results are returned
immediately from the cache without touching opam, and existing switches are
reused rather than recreated.

## Limitations

**OCaml 4 and OCaml 5 minimums that differ.** When a dependency has a lower
minimum on OCaml 4 than on OCaml 5, only the OCaml 5 (higher) minimum is
written. opam's `pkg:var` package-variable filters are not reliably evaluated
during dependency solving, so per-major-version bounds cannot be expressed
(opam lint error E29).
The tool prints which packages were affected and automatically checks whether
the higher (OCaml 5) minimums can be installed on OCaml 4, warning if they
cannot.

**`with-doc` dependencies are not probed.** Only runtime and `with-test`
dependencies are tested. `with-doc` lower bounds must be set manually.

**`generate_opam_files` projects.** If the `dune-project` file contains
`(generate_opam_files true)`, the `.opam` file is managed by dune and would
be overwritten on the next build. `opam-minver` detects this and prints the
discovered bounds without writing them, even if `--write` is passed.

**Only a single `.opam` file is supported.** If multiple .opam files exist in
the project directory, `opam-minver` will halt with an error.

**Stale cache.** If you change the project's compatibility requirements: for
example, dropping OCaml 4 support, raising a lower bound manually, or adding a
new dependency, delete `opam-minver.json` before re-running. Without this,
the cached results from the previous run may prevent the new bounds from being
found correctly.

**The project must build.** `opam-minver` first verifies that the project
builds and tests pass with the currently active compiler. If it does not, the
run aborts. Switch to a compiler under which the project builds before running
the tool.

## License

MIT
