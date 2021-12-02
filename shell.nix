let
  pkgs = import ./nix/default.nix { };
  sources = import ./nix/sources.nix;
  pkgs_2105 = import sources.nixpkgs_2105 {
      overlays = [];
      config = {};
  };
in
pkgs.mkShell {
  # GNU ls has different CLI options than Darwin ls.
  shellHooks = ''
    alias ll='ls -alh --color=auto'
    alias ls='ls -ah --color=auto'
  '';

  # Disable Bazel's Xcode toolchain detection which would configure compilers
  # and linkers from Xcode instead of from PATH.
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN = 1;

  # Set UTF-8 locale.
  LANG = "C";
  LC_CTYPE = "UTF-8";

  buildInputs = with pkgs; [
    git
    # for SSL downloads
    cacert
    # for rules_nix
    nix
    # buildifier, buildozer and unused-deps
    bazel-buildtools
    pkgs_2105.bazel_4
    openjdk11
    # Haskell stuff
    ghc_8_10_7
    haskell-language-server
    nixpkgs-fmt
    ormolu
  ];
}
