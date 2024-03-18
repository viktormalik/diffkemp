![Build](https://github.com/viktormalik/diffkemp/actions/workflows/ci.yml/badge.svg?branch=master)

# DiffKemp

DiffKemp is a framework for automatic static analysis of semantic differences
between different versions of projects written in C, with main focus on the
Linux kernel.

The main use-case of DiffKemp is to compare selected functions and configuration
options in two versions of a project and to report any discovered semantic
differences.

## About

The main focus of DiffKemp is high scalability, such that it can be applied to
large-scale projects containing a lot of code. To achieve that, the analysed
functions are first compiled into LLVM IR, then several code transformations are
applied, and finally the comparison itself is performed.

Wherever possible, DiffKemp tries to compare instruction-by-instruction (on LLVM
IR instructions) which is typically sufficient for most of the code. When not
sufficient, DiffKemp tries to apply one of the built-in or user-supplied
*semantics-preserving patterns*. If a semantic difference is still discovered,
the relevant diffs are reported to the user.

Note that DiffKemp is incomplete it its nature, hence may provide false positive
results (i.e. claiming that functions are not equivalent even though they are).
This especially happens with complex refactorings.

## Installation

There are two options to install DiffKemp, either build from source or use a
prepared RPM package for Fedora.

### Install from source

Currently, DiffKemp runs on Linux and needs the following software installed:
* Clang and LLVM (supported versions are 9, 10, 11, 12, 13, 14, 15)
* Z3 SMT solver and its C++ bindings (packages `z3` and `z3-devel` in Fedora)
* Python 3 with CFFI (package `python3-cffi` in Fedora and Debian)
* Python packages from `requirements.txt` (run `pip install -r requirements.txt`)
* CScope (when comparing versions of the Linux kernel)
* GoogleTest (gtest) for running the C++ tests (can be vendored by using
  `-DVENDOR_GTEST=On` in the `cmake` command)

Additionally, to build manually, you need to install the following tools:
* CMake
* Ninja build system

Build can be done by running:

    mkdir build
    cd build
    cmake .. -GNinja -DCMAKE_BUILD_TYPE=Release
    ninja
    cd ..

    pip install -e .

The DiffKemp binary is then located in `bin/diffkemp`.

For running the result viewer, it is necessary to have installed:
* Node.js (>= 14.x)
* npm (8)

When building with `cmake`, use `-DBUILD_VIEWER=On` to install necessary
packages and build an optimized version of the result viewer.

### Install from RPM

Alternatively, you can use a prepared RPM package for Fedora that can be
installed from our Copr repository:
[https://copr.fedorainfracloud.org/coprs/viktormalik/diffkemp/](https://copr.fedorainfracloud.org/coprs/viktormalik/diffkemp/)

## Usage
DiffKemp runs in two phases:

- **Snapshot generation** compiles the compared project versions into LLVM IR
  and creates so-called *snapshots* which contain the relevant LLVM IR files and
  additional metadata.

  There are several options for snapshot generation:
  - ```
    diffkemp build PROJ_DIR SNAPSHOT_DIR [SYMBOL_LIST]
    ```
    is the default snapshot generation command for `make`-based projects. It
    takes the project located in `PROJ_DIR`, builds it into LLVM IR, and creates
    a snapshot for comparing semantics of functions from `SYMBOL_LIST` (if no
    list is given, all exported functions from the project are considered). The
    snapshot is stored in `SNAPSHOT_DIR`. Warning: if `SNAPSHOT_DIR` exists, it
    will be rewritten.

    The command should be run twice, once for each of the compared versions.  It
    also has additional options to configure the project build, see `diffkemp
    build --help` for the complete list.

    The command can be also used to generate a snapshot from a single C file.
    In this case, the path to the file should be given in place of `PROJ_DIR`.

  - ```
    diffkemp build-kernel KERNEL_DIR SNAPSHOT_DIR SYMBOL_LIST
    ```
    is a command similar to `build` which is specialized for building snapshots
    from the Linux kernel. Its main advantage is that it does not build the
    entire kernel, only the files containing functions from `SYMBOL_LIST`. The
    kernel source to build must be properly configured (by `make prepare`) and
    all the tools necessary for building kernel must be installed.

  - ```
    diffkemp llvm-to-snapshot PROJ_DIR LLVM_FILE SNAPSHOT_DIR SYMBOL_LIST
    ```
    can be used if the project is already compiled into a single LLVM IR file.
    The file name is given in `LLVM_FILE` and must be relative to `PROJ_DIR`.
    The remaining options are the same as for the other commands.

- **Semantic comparison** takes two snapshots and compares them for semantic
  equality. It is invoked via:
  ```
  diffkemp compare SNAPSHOT_DIR_1 SNAPSHOT_DIR_2
  ```

  To show syntactic diffs of the discovered differences, use the `--syntax-diff`
  option. The diffs are stored in separate files (one file for each compared
  function that is different) in a newly created directory. The name of the
  directory can be specified using the `-o` option, otherwise it is generated
  automatically. The `--stdout` option causes the diffs to be printed to
  standard output.

Additionally, you can run **result viewer** to get a visualisation of the found
differences.

- **Result viewer** takes the directory with the output of the compare phase. 
  It is invoked via:
  ```
  diffkemp view COMPARE_OUTPUT_DIR
  ```

  It prepares the necessary files and runs a static server. The command displays the URL that you can use to access the result viewer.

### Comparing sysctl options

Apart from comparing specific functions, DiffKemp supports comparison of
semantics of sysctl options. The list of the options to compare can be passed
via `SYMBOL_LIST` to the `build-kernel` command. In such case, use `--sysctl`
switch to generate snapshot for sysctl parameter comparison. The `compare`
command is used in normal way.

Sysctl option comparison compares semantics of the proc handler function and
semantics of all functions using the data variable that the sysctl option sets.

It is possible to use patterns to specify a number of multiple sysctl options at
once such as:
* `kernel.*`
* `kernel.{sysctl-1|sysctl-2}`

Currently, these sysctl option groups are supported: `kernel.*`,
`vm.*`, `fs.*`, `net.core.*`, `net.ipv4.conf.*`.

## Important implementation details

The core of DiffKemp is the *SimpLL* component, written in C++ for performance
reasons. The user inputs and comparison results are handled by Python for
simplicity. SimpLL performs several important steps:

- Simplification of the compared programs by applying multiple code
  transformations such as dead code elimination, indirect calls abstraction,
  etc.
- Debug info parsing to collect information important for the comparison.
  DiffKemp needs the analysed project to be built with debugging information in
  order to work properly.
- The comparison itself is mostly done instruction-by-instruction. In addition,
  DiffKemp handles semantics-preserving changes that adhere to one of the
  built-in patterns. Additional patterns can be specified manually and passed to
  the `compare` command.
- See publications on the bottom of this page for futher information on DiffKemp
  internals.

## Development

### Docker

For a better developer experience, there is a development container image
prepared that can be retrieved from DockerHub:
[https://hub.docker.com/r/viktormalik/diffkemp-devel/](https://hub.docker.com/r/viktormalik/diffkemp-devel/)

After that, the container can be run using

    docker/diffkemp-devel/run-container.py [--llvm-version LLVM_VERSION]
                                           [--build-dir BUILD_DIR]
                                           [--diffkemp-dir DIFFKEMP_DIR]
                                           [--image IMAGE]

The script mounts the current directory as a volume inside the container.
Then it automatically builds SimpLL in `BUILD_DIR` (default is "build") using
`LLVM_VERSION` (default is the newest supported version) and installs DiffKemp.

If running multiple containers at the same time, you need to specify a unique
`BUILD_DIR` for each one.

If running the container from a different directory than the root DiffKemp
directory, you need to specify where DiffKemp is located using the
`--diffkemp-dir` option.

By default, the DockerHub image is used, but a custom image may be set using
the `--image` option.

### Nix

In addition to container environment, we provide [Nix
flakes](https://nixos.wiki/wiki/Flakes) for building and testing DiffKemp.

First, it is necessary to [install Nix](https://nixos.org/download.html) and
enable flakes:
```
$ mkdir -p ~/.config/nix
$ echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

Then, building DiffKemp is as simple as running one of the following commands:
```
$ nix build                    # <- uses latest LLVM
$ nix build .#diffkemp-llvm14
```
This will create a new folder `result/` containing the pre-built DiffKemp which
can be then executed by:
```
$ result/bin/diffkemp ...
```

Similarly, it is also possible to use Nix as a development environment:
```
$ nix develop                   # <- uses latest LLVM
$ nix develop .#diffkemp-llvm14
```
This will enter a development shell with all DiffKemp dependencies
pre-installed. You can then follow the [standard build
instructions](#install-from-source) to build and install DiffKemp. The only
difference is that it is not possible to run `pip install` inside Nix shell
(because of the way Nix works) and it is necessary to use the built-in
`setuptoolsShellHook` function instead.

We also provide a special Nix environment for retrieving and preparing kernel
versions necessary for running regression tests (see below for details).

### Tests

The project contains unit and regression testing using pytest that can be run
by:

    pytest tests

The tests require the sources of the following kernel versions to be stored and
configured in `kernel/linux-{version}` directories:
* 3.10 (upstream kernel)
* 4.11 (upstream kernel)
* 3.10.0-514.el7 (CentOS 7.3 kernel)
* 3.10.0-693.el7 (CentOS 7.4 kernel)
* 3.10.0-862.el7 (CentOS 7.5 kernel)
* 3.10.0-957.el7 (CentOS 7.6 kernel)
* 4.18.0-80.el8 (RHEL 8.0 kernel)
* 4.18.0-147.el8 (RHEL 8.1 kernel)
* 4.18.0-193.el8 (RHEL 8.2 kernel)
* 4.18.0-240.el8 (RHEL 8.3 kernel)

The required configuration of each kernel can be done by running:

    make prepare
    make modules_prepare
    
The [rhel-kernel-get](https://github.com/viktormalik/rhel-kernel-get) script can also be used
to download and configure the aforementioned kernels.

Since CentOS 7 kernels require a rather old GCC 7, the most convenient way to
download the kernels is to use the prepared Nix environment by running
```
$ nix develop .#test-kernel-buildenv
```
and using `rhel-kernel-get` inside the environment to retrieve the above
kernels.

The result viewer contains unit tests and integration tests
which can be run by:

    cd view && npm test -- --watchAll
    
## Contributors

The list of code and non-code contributors to this project, in pseudo-random
order:
- Viktor Malík
- Tomáš Glozar
- Tomáš Vojnar
- Petr Šilling
- Pavol Žáčik
- Lukáš Petr
- Tatiana Malecová
- Jakub Rozek

## Publications and talks

There is a number of publications and talks related to DiffKemp:
- ICST'21 [paper](https://ieeexplore.ieee.org/document/9438578)
  and [talk](https://zenodo.org/record/4658966):  
  V. Malík and T. Vojnar, "Automatically Checking Semantic Equivalence between
  Versions of Large-Scale C Projects," 2021 14th IEEE Conference on Software
  Testing, Verification and Validation (ICST), 2021, pp. 329-339.
- NETYS'22
  [paper](https://link.springer.com/chapter/10.1007/978-3-031-17436-0_18) and
  [talk](https://www.youtube.com/watch?v=FPOUfgorF8s):  
  Malík, V., Šilling, P., Vojnar, T. (2022). Applying Custom Patterns in
  Semantic Equality Analysis. In: Koulali, MA., Mezini, M. (eds) Networked
  Systems. NETYS 2022.
- [DevConf.CZ'19 talk](https://www.youtube.com/watch?v=PUZSaLf9exg).
