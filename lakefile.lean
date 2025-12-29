import Lake
open Lake DSL System

package ask where
  version := v!"0.1.0"

require parlance from git "https://github.com/nathanial/parlance" @ "v0.0.1"
require oracle from git "https://github.com/nathanial/oracle" @ "v0.0.1"
require chronicle from git "https://github.com/nathanial/chronicle" @ "v0.0.1"

-- Platform-specific curl library paths (required by wisp via oracle)
def curlLinkArgs : Array String :=
  if Platform.isOSX then
    #["-L/opt/homebrew/opt/curl/lib",
      "-L/opt/homebrew/lib",
      "-L/usr/local/lib",
      "-L/opt/homebrew/anaconda3/lib",
      "-lcurl",
      "-Wl,-rpath,/opt/homebrew/opt/curl/lib",
      "-Wl,-rpath,/opt/homebrew/lib",
      "-Wl,-rpath,/opt/homebrew/anaconda3/lib",
      "-Wl,-rpath,/usr/local/lib"]
  else if Platform.isWindows then
    #["-lcurl"]
  else
    #["-lcurl", "-Wl,-rpath,/usr/lib", "-Wl,-rpath,/usr/local/lib"]

lean_lib Ask where
  globs := #[.submodules `Ask]

@[default_target]
lean_exe ask where
  root := `Main
  moreLinkArgs := curlLinkArgs
