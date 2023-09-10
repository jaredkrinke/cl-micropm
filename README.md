# cl-micropm

A very minimalist "package manager" for Common Lisp in under 200 LOC.


## How it works

1. Load `cl-micropm.lisp`. Since it's a single file, you don't need to have ASDF
   load the system definition, but it's there if you need it.

2. Run `(micropm:setup <your-system-name>)`. It doesn't directly use quicklisp,
   but this will fetch the quicklisp system sources and build the quicklisp
   dependency index, and then fetch the system dependencies using these data. By
   default, it will create git submodules in the `lisp-systems` folder.

3. Tweak the `lisp-systems` to your needs, whether you need to freeze a
   dependency on a particular branch or commit, etc.

Once all your deps are setup in `lisp-systems`, you can just run the normal
flow with `(micropm:setup-asdf-registry)`.

You can also skip `(micropm:setup <your-system-name>)` and just manually add the
deps into `lisp-systems` if you want to do that, by reading the quicklisp
dependency index.

**Note**: The old version used an `.envrc` file and pulled the dependencies from
quicklisp in a Docker container. See the `old` branch if you want to use that
instead.



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
