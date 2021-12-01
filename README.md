# dimitrije's solutions for Advent of Code 2021

My solutions for the 2021 Advent of Code (https://adventofcode.com). Why not,
it's good for your mental health.

## How to run

With `nix`:

```bash
$ nix-shell
$ bazel run //app:main -- <day>
```

## How to test

Run all tests with:

```bash
$ nix-shell
$ bazel test "//test:*"
```
