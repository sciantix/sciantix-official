# SCIANTIX training repository

This branch is designed for educational purposes and provides a self-contained environment for learning SCIANTIX.

## Repository structure

* **`code/`**: SCIANTIX source code (v2.0+) and build files.
* **`material/`**: Official training cases (input/output examples).
* **`slides/`**: Theoretical support and step-by-step tutorials.

---

## Environment setup (Windows)

### Option A: WSL (Highly recommended and supported)

This is the standard environment for SCIANTIX. It provides the best performance and compatibility with automated scripts.

1. Open PowerShell as Administrator and run:
```powershell
wsl --install

```


2. Restart your computer.
3. In the Ubuntu terminal, install the development suite:
```bash
sudo apt update && sudo apt install -y build-essential cmake git

```



### Option B: Code::Blocks (Legacy)

You can compile SCIANTIX natively using [Code::Blocks](https://www.codeblocks.org/downloads/binaries/) (download the version with `mingw-setup.exe`).
*Note: This option is provided for convenience but is **not actively supported** for this training.*

---

## Environment setup (macOS and Linux)

Ensure you have a C++ compiler and CMake installed:

* **Linux**: `sudo apt install build-essential cmake git`
* **macOS**: `xcode-select --install` and `brew install cmake`.

---

## Download and compile

1. **Clone the repository**:
```bash
git clone -b training https://github.com/sciantix/sciantix-official.git
cd sciantix-official

```


2. **Compile the code via terminal (WSL/Linux/macOS)**:
The compilation process produces an executable named **`sciantix.x`** inside the `build` folder.
```bash
cd code
mkdir build && cd build
cmake ..
make
cp sciantix.x ../../material/
cd ../..

```


3. **Compile via Code::Blocks (Windows)**:
The compilation produces an executable named **`sciantix.exe`**. You can run the simulations by simply double-clicking the executable inside a case folder.

---

## Available training cases

The **`material/`** directory contains scenarios to explore fission gas behavior:

| Folder | Description |
| --- | --- |
| `1_baker1977_stationary` | Intra-granular diffusion, trapping, and re-solution. |
| `2_white2004_transient` | Grain-boundary swelling, saturation, and burst release. |
| `3_uo2_hbs` | High-burnup structure (HBS) formation. |

### How to run

* **On WSL/Linux/macOS**: Navigate into a case folder and execute:
```bash
cd material
cp sciantix.x 1_baker1977_stationary/
cd 1_baker1977_stationary
./sciantix.x

```


* **On Windows (Code::Blocks)**: Copy the `sciantix.exe` into the specific case folder and double-click it.

---

## Useful links

* [SCIANTIX documentation](https://sciantix.github.io/sciantix-official/)
* [Main repository](https://github.com/sciantix/sciantix-official)