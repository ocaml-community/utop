# Contributing

Thanks for contributing to UTop!

## Setting up local switches

UTop comes from a `dune-workspace.dev` file to test it across all supported
versions.

Run `make create-switches` to create all the required switches.

Now you can run `dune` with the `--workspace dev-workspace.dev` flag to run
the same command across all the workspaces. The `make
all-supported-ocaml-versions` command will build the project with this setup.

## Compatibility Across Versions

Some code will be different from one version of OCaml to the next. If you find
some that does, please abstract it away using the `UTop_compat` module.

For example, the `Load_path.get_paths ()` function has changed recently to
return a record with shape `{ visible: string list; hidden: string list }`, but
this function used to return a single `string list`.

Defining this function using pre-processor macros allows us to give the same
function two different bodies on different version of the language.

```ocaml
let get_load_path () =
#if OCAML_VERSION >= (5, 2, 0)
  Load_path.((get_paths ()).visible)
#else
  Load_path.get_paths ()
#endif
```
