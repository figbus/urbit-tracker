{
  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          urbit-app-base = pkgs.stdenv.mkDerivation rec {
            pname = "urbit-app-base";
            version = "2.123";

            src = pkgs.fetchFromGitHub {
              owner = "urbit";
              repo = "urbit";
              rev = "urbit-os-v${version}";
              sha256 = "Jdysf/Nt+GrvMW7R1ldFX8SIKUiocncN6cp6wN1iNOQ=";
            };

            dontBuild = true;

            installPhase = ''
              mkdir $out
              cp -r pkg/base-dev/* $out
              cp -r pkg/garden-dev/* $out
            '';
          };
        in
        rec {
          packages.urbit-tracker = pkgs.stdenv.mkDerivation {
            name = "urbit-tracker";

            src = ./src;

            installPhase = ''
              mkdir $out
              cp -r * $out
              cp -r ${urbit-app-base}/* $out
            '';
          };
          defaultPackage = packages.urbit-tracker;
        }
      );
}
