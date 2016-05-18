{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ad, base, base-unicode-symbols, binary
      , bytestring, containers, data-reify, mtl, pretty-show, QuickCheck
      , random, reflection, simple-reflect, split, stdenv
      }:
      mkDerivation {
        pname = "mnist";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ad base base-unicode-symbols binary bytestring containers
          data-reify mtl pretty-show random reflection simple-reflect split
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base QuickCheck ];
        homepage = "http://github.com/sleexyz/mnist#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
