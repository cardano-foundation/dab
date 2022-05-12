{ pkgs
, dockerTools
, runtimeShell
, writeScriptBin
, project }:
let
  baseImage = dockerTools.buildImage {
    name = "base-env";
    config.Env = [
      "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    contents = with pkgs; [
      bashInteractive
      cacert
      coreutils
      iana-etc
    ];
  };

  entry-point = writeScriptBin "entry-point" ''
    #!${runtimeShell}
    cat /etc/resolv.conf
    ${project.chain-watcher.components.exes.chain-watcher-blockfrost}/bin/chain-watcher-blockfrost
  '';
in
 dockerTools.buildImage {
    name = "chain-watcher";
    fromImage = baseImage;
    # tag = gitrev;
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [
      project.chain-watcher.components.exes.chain-watcher-blockfrost
    ];
    config = {
      EntryPoint = [
        "${entry-point}/bin/entry-point"
      ];
    };
  }
