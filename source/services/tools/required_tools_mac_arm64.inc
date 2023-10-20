REQUIRED_TOOLS : TToolsInfo = (
  (Name: 'jdk'; Description: 'Eclipse Temurin'; Version: '21-35';
    Url: 'https://api.adoptium.net/v3/binary/version/jdk-21-35/mac/aarch64/jdk/hotspot/normal/eclipse?project=jdk'),

  (Name: 'sdk'; Description: 'Android Software Development Kit'; Version: '32';
    Url: 'https://dl.google.com/android/repository/commandlinetools-mac-10406996_latest.zip';
    TermsOfUse: 'https://developer.android.com/studio/terms';
    Args: ['"platforms;android-32"', '"build-tools;32.0.0"', 'platform-tools'{, 'tools'}];
    Requires: [
      @REQUIRED_TOOLS[INDEX_JDK_21_35],
      @REQUIRED_TOOLS[INDEX_CMD_TOOLS_10406996]
    ])
);