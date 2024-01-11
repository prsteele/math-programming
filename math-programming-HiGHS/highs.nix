{ cmake, fetchFromGitHub, stdenv }:
stdenv.mkDerivation {
  name = "HiGHS";
  version = "1.6.0";
  src = fetchFromGitHub {
    owner = "ERGO-Code";
    repo = "HiGHS";
    rev = "v1.6.0";
    sha256 = "sha256-Wa5ivUJk0t58FhZD0zy0zxHHj4/p8e9WcxXwu5zenxI=";
  };

  nativeBuildInputs = [ cmake ];

  postInstall =
    # The headers get sent to include/highs/ instead of just include/, but
    # the headers themselves don't use the highs/ prefix
    ''
      cp -r $out/include/highs/. $out/include/;
      rm -r $out/include/highs;
    '';
}
