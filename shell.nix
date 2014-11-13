let pkgs = import <nixpkgs> {};
    myHaskellPackages = pkgs.myHaskellPackages.override {
      extension = self: super: {
        memento = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation myHaskellPackages.memento (attrs: {
   buildInputs = [ myHaskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
