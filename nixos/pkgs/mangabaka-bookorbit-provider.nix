{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "mangabaka-provider";
  version = "0.1.0-unstable-2026-11-10";

  src = fetchFromGitHub {
    owner = "bookorbit";
    repo = "bookorbit-metadata-providers";
    tag = "mangabaka-v0.1.0";
    hash = "sha256-IkbgHWuEbZLXvV4Tzu35XqbyUceLlEvN4LooXWoKdM0=";
  };

  # The crate lives in a subdirectory of the repo
  sourceRoot = "${src.name}/providers/mangabaka";

  cargoHash = "sha256-yaXaUmABSVueDLZB5XhU85OH4YhfzUoHB2kbwfvXEuU=";

  # reqwest uses rustls, so no openssl/pkg-config needed.
  # Tests may try to hit the network; disable if they fail in the sandbox:
  # doCheck = false;

  meta = {
    description = "MangaBaka metadata provider for BookOrbit";
    homepage = "https://github.com/bookorbit/bookorbit-metadata-providers";
    mainProgram = "mangabaka-provider";
  };
}
