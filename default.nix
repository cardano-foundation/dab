########################################################################
# default.nix -- The top-level nix build file for dab
#
# This file defines various attributes that are used for building and
# developing dab.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   dab: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs dab;
  project = dab.haskell.project;
in
{
  inherit pkgs dab;

  inherit project;
}
