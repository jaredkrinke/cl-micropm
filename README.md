# cl-micropm

A very minimalist "package manager" for Common Lisp in under 150 LOC.


## How it works

1. Load `cl-micropm.lisp`.
2. Run `(micropm:setup <your-system-name>)`. It uses a snapshot of Quicklisp's dependency index to fetch system dependencies. By default, it will create git submodules in `../deps/`. Use `:clone t` to clone instead of creating submodules and `:dry-run t` to do a "dry run" (print commands that will be run, but don't actually run them).

## Goals

- It should be loadable as a single file.
- The code should be easily auditable and readable, and therefore small in size
  (< 200 LOCs).
- It should be decentralized. No quicklisp or ultralisp, just git, http(s), etc.
- Dependendies should not be global, each project may have a local version that's different and may
  want to pin that version.
- It should be easy to work on projects that use different versions of a package, maybe even at the
  same time, without too much hassle.
- It should be easy to set up.
- It should be easy to understand what it's doing and how it works.
- It should be possible to just freeze the dependencies (separate fetching and loading commands).



## License

See [LICENSE](./LICENSE)
