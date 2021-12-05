load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
)

def haskell_spec_test(name, deps, srcs):
    haskell_test(
        name = name,
        srcs = srcs + ["Spec.hs"],
        main_function = "Spec.main",
        tools = ["@hspec-discover//:bin"],
        ghcopts = [
            "-threaded",
            "-rtsopts",
            "-with-rtsopts=-N",
            "-DHSPEC_DISCOVER=$(execpath @hspec-discover//:bin)",
        ],
        deps = deps + ["//test:hspec"],
        tags = ["test"],
        visibility = [
            "//visibility:public",
        ],
    )
