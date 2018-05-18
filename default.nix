{ system ? builtins.currentSystem }: # TODO: Get rid of this system cruft
with import ./.obelisk/impl { inherit system; };
project ./. ({ ... }: {
  android.applicationId = "ca.srid.riceneggs";
  android.displayName = "Rice 'n eggs";
  ios.bundleIdentifier = "ca.srid.riceneggs";
  ios.bundleName = "Rice 'n eggs";
})
