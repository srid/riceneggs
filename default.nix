{ system ? builtins.currentSystem }: # TODO: Get rid of this system cruft
with import ./.obelisk/impl { inherit system; };
project ./. ({ ... }: {
  android.applicationId = "ca.srid.riceneggs";
  android.displayName = "Rice n eggs";
  android.releaseKey = 
    { storeFile = /home/srid/code/riceneggs/myandroidkey.jks;
      storePassword = "abcd1234";
      keyAlias = "myandroidalias";
      keyPassword = "abcd1234";
    };
  ios.bundleIdentifier = "ca.srid.riceneggs";
  ios.bundleName = "Rice n eggs";
})
