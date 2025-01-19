const path = require("path");
const fs = require("fs");

const installMacLinuxBinary = (binary) => {
  const source = path.join(__dirname, binary);
  if (fs.existsSync(source)) {
    // mac and linux support extension-less executables
    // so just overwrite the shell script
    const target = path.join(__dirname, "ppx");
    fs.renameSync(source, target);

    // The ppx should be executable in the bundle, but just in case
    fs.chmodSync(target, 0777);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

const installWindowsBinary = () => {
  const source = path.join(__dirname, "ppx-windows.exe");
  if (fs.existsSync(source)) {
    const target = path.join(__dirname, "ppx.exe");
    fs.renameSync(source, target);

    // windows scripts use a different file extension to executables
    // so we delete the script to make sure windows uses the exe now
    const windowsScript = path.join(__dirname, "ppx.cmd");
    fs.unlinkSync(windowsScript);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

switch (process.platform) {
  case "linux":
    switch (process.arch) {
      case "amd64":
        installMacLinuxBinary("ppx-linux.exe");
        break;
      case "arm64":
        installMacLinuxBinary("ppx-linux-arm64.exe");
        break;
      default:
        // This won't break the installation because the `ppx` shell script remains
        // but that script will throw an error in this case anyway
        console.warn(
          `No release available for platform "${process.platform}" arch "${process.arch}"`
        );
        process.exit(1);
    }
    break;
  case "darwin":
    installMacLinuxBinary("ppx-osx.exe");
    break;
  case "win32":
    installWindowsBinary();
    break;
  default:
    // This won't break the installation because the `ppx` shell script remains
    // but that script will throw an error in this case anyway
    console.warn(`No release available for "${process.platform}"`);
    process.exit(1);
}
