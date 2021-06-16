{ stdenv, lib, fetchurl, pam ? null, libX11, libXext, libXinerama
, libXdmcp, libXt, autoconf }:

stdenv.mkDerivation rec {
  name = "xlockmore-5.66";

  src = fetchurl {
    url = "http://sillycycle.com/xlock/${name}.tar.xz";
    sha256 = "sha256-WXalw2YoKNFFIskOBvKN3PyOV3iP3gjri3pw6e87q3E=";
    curlOpts = "--user-agent 'Mozilla/5.0'";
  };

  # Optionally, it can use GTK.
  buildInputs = [ pam libX11 libXext.dev libXinerama.dev libXdmcp libXt ];
  nativeBuildInputs = [ autoconf ];

  # Don't try to install `xlock' setuid. Password authentication works
  # fine via PAM without super user privileges.
  configureFlags =
    [ "--disable-setuid"
    ] ++ (lib.optional (pam != null) "--enable-pam");

  postPatch =
    let makePath = p: lib.concatMapStringsSep " " (x: x + "/" + p) buildInputs;
        inputs = "${makePath "lib"} ${makePath "include"}";
    in ''
      sed -i 's,\(for ac_dir in\),\1 ${inputs},' configure.ac
      sed -i 's,/usr/,/no-such-dir/,g' configure.ac
      configureFlags+=" --enable-appdefaultdir=$out/share/X11/app-defaults"
    '';
  preConfigure = ''
    # needed to apply changes to configure.ac in postPatch step
    autoconf
  '';

  hardeningDisable = [ "format" ]; # no build output otherwise

  meta = with lib; {
    description = "Screen locker for the X Window System";
    homepage = "http://sillycycle.com/xlockmore.html";
    license = licenses.gpl2;
    maintainers = with maintainers; [ pSub ];
    platforms = platforms.linux;
  };
}
