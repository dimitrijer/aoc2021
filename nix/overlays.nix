self: super: {
  # Make ghc8107 the default package set for Haskell.
  haskellPackages = super.haskell.packages.ghc8107.override {
    overrides = self: super: { };
  };
  # Override ghc, adding all project dependencies as toolchain packages.
  ghc = self.haskellPackages.ghcWithPackages (
    ps: with ps; [
      array
      split
      hspec
    ]
  );
  haskell-language-server = super.haskell-language-server.override {
    supportedGhcVersions = [ "8107" ];
  };
}
