{ stdenv, lib, fetchurl, makeWrapper
, cups
, dpkg
, a2ps, ghostscript, gnugrep, gnused, coreutils, file, perl, which
}:

stdenv.mkDerivation rec {
  name = "hll3230cdw-cups-${version}";
  version = "1.0.2-0";

  src = fetchurl {
    url = "https://download.brother.com/welcome/dlf103925/hll3230cdwpdrv-1.0.2-0.i386.deb";
    sha256 = "sha256-nUmrxYS/Irw4FRBhijQQfq1qsUVitRgx/v1gCZR6pak=";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ cups ghostscript dpkg a2ps ];

  unpackPhase = ":";

  installPhase = ''
    dpkg-deb -x $src $out

    substituteInPlace $out/opt/brother/Printers/hll3230cdw/cupswrapper/brother_hll3230cdw_printer_en.ppd \
    --replace '"Brother HL-L3230CDW' '"Brother HL-L3230CDW (modified)'

    substituteInPlace $out/opt/brother/Printers/hll3230cdw/lpd/filter_hll3230cdw \
    --replace /opt "$out/opt" \
    --replace /usr/bin/perl ${perl}/bin/perl \
    --replace "BR_PRT_PATH =~" "BR_PRT_PATH = \"$out/opt/brother/Printers/hll3230cdw/\"; #" \
    --replace "PRINTER =~" "PRINTER = \"HLL3230CDW\"; #"

    # FIXME : Allow i686 and armv7l variations to be setup instead.
    _PLAT=x86_64
    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
    $out/opt/brother/Printers/hll3230cdw/lpd/brhll3230cdwfilter
    ln -s $out/opt/brother/Printers/hll3230cdw/lpd/brhll3230cdwfilter $out/opt/brother/Printers/hll3230cdw/brhll3230cdwfilter

    for f in \
    $out/opt/brother/Printers/hll3230cdw/cupswrapper/brother_lpdwrapper_hll3230cdw \
    $out/opt/brother/Printers/hll3230cdw/cupswrapper/cupswrapperhll3230cdw \
    ; do
    #substituteInPlace $f \
    wrapProgram $f \
    --prefix PATH : ${lib.makeBinPath [
      coreutils ghostscript gnugrep gnused
    ]}
    done

    # Hack suggested by samueldr.
    sed -i"" "s;A4;Letter;g" $out/opt/brother/Printers/hll3230cdw/inf/brhll3230cdwrc

    mkdir -p $out/lib/cups/filter/
    ln -s $out/opt/brother/Printers/hll3230cdw/lpd/filter_hll3230cdw $out/lib/cups/filter/brother_lpdwrapper_hll3230cdw

    mkdir -p $out/share/cups/model
    ln -s $out/opt/brother/Printers/hll3230cdw/cupswrapper/brother_hll3230cdw_printer_en.ppd $out/share/cups/model/

    wrapProgram $out/opt/brother/Printers/hll3230cdw/lpd/filter_hll3230cdw \
    --prefix PATH ":" ${ lib.makeBinPath [ ghostscript a2ps file gnused gnugrep coreutils which ] }
  '';

  meta = with lib; {
    homepage = "https://www.brother.com/";
    description = "Brother HL-L3230CDW combined print driver";
    license = licenses.unfree;
    platforms = [
      "x86_64-linux"
    ];
  };
}
