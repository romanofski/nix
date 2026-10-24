{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  pkg-config,
  freetype,
  SDL2,
  SDL2_image,
  libGL,
  libGLU,
  glew,
  libtiff,
  libossp_uuid,
  yaml-cpp,
}:

stdenv.mkDerivation rec {
  pname = "glsmac";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "afwbkbc";
    repo = "glsmac";
    rev = "v0.3-db4dbc9";
    hash = "sha256-kea9EohSUWRHu/xYBuQrUwJex1zNYzYRjeANgobcDl8=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
  ];

  buildInputs = [
    SDL2
    SDL2_image
    freetype
    glew
    libGL
    libGLU
    libossp_uuid
    libtiff
    yaml-cpp
  ];

  cmakeBuildType = "Release";

  patches = [
    ./glsmac-version.patch
  ];
  cmakeFlags = [
    "-DVENDORED_DEPENDENCIES=NO"
    "-DGLSMAC_LAST_COMMIT=${src.rev}"
  ];
  postPatch = ''
    substituteInPlace CMakeLists.txt \
    --replace-fail 'ossp-uuid' 'uuid'
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 bin/GLSMAC "$out/bin/glsmac"
    runHook postInstall
  '';

  enableParallelBuilding = true;

  meta = {
    description = "Game engine for Sid Meier's Alpha Centauri";
    platforms = lib.platforms.linux;
    mainProgram = "glsmac";
  };
}
