workspace(name = "hrepl")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "rules_haskell",
    remote = "https://github.com/tweag/rules_haskell",
    commit = "c1625027dd72c6aa17c8b48c176e7a6a787d406e",
    shallow_since = "1580119964 -0800",
)
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)
haskell_register_ghc_bindists(
    version = "8.6.5",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
stack_snapshot(
    name = "stackage",
    snapshot = "lts-14.18",
    packages= [
        "Cabal",
        "HUnit",
        "async",
        "atomic-primops",
        "base",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "extra",
        "filepath",
        "ghc",
        "ghc-typelits-knownnat",
        "haskell-src-exts",
        "hspec-expectations",
        "lens-family",
        "microlens",
        "optparse-applicative",
        "process",
        "proto-lens",
        "proto-lens-protoc",
        "strict",
        "temporary",
        "template-haskell",
        "transformers",
        "test-framework",
        "test-framework-hunit",
        "text",
        "unix",
        "vector",
    ],
    tools = [
        "@happy",
    ],
)

register_toolchains(
    ":proto_toolchain",
)

http_archive(
    name = "proto-lens-protoc",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "proto-lens-protoc",
    srcs = glob(["**"]),
    deps = [
      "@stackage//:base",
      "@stackage//:bytestring",
      "@stackage//:containers",
      "@stackage//:lens-family",
      "@stackage//:proto-lens",
      "@stackage//:proto-lens-protoc",
      "@stackage//:text",
    ],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "161dcee2aed780f62c01522c86afce61721cf89c0143f157efefb1bd1fa1d164",
    strip_prefix = "proto-lens-protoc-0.5.0.0",
    urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.5.0.0/proto-lens-protoc-0.5.0.0.tar.gz"],
)

http_archive(
    name = "happy",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "happy", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "22eb606c97105b396e1c7dc27e120ca02025a87f3e44d2ea52be6a653a52caed",
    strip_prefix = "happy-1.19.10",
    urls = ["http://hackage.haskell.org/package/happy-1.19.10/happy-1.19.10.tar.gz"],
)
