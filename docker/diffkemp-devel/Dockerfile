FROM ubuntu:focal
MAINTAINER Viktor Malik <vmalik@redhat.com>
ENV LLVM_VERSION=16
RUN apt-get update && \
    apt-get install -y \
      apt-transport-https \
      software-properties-common \
      wget
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    add-apt-repository -y ppa:ubuntu-toolchain-r/test && \
    add-apt-repository -y ppa:deadsnakes/ppa && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-9 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-10 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-11 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-12 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-13 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-14 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-15 main" && \
    add-apt-repository -y "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-16 main"
RUN apt-get update && \
    apt-get install -y \
      autoconf \
      bc \
      bison \
      bzip2 \
      clang-9 \
      clang-10 \
      clang-11 \
      clang-12 \
      clang-13 \
      clang-14 \
      clang-15 \
      clang-16 \
      clang-format-8 \
      cmake \
      cpio \
      cscope \
      curl \
      libelf-dev \
      libgmp-dev \
      libssl-dev \
      libz3-dev \
      libgtest-dev \
      flex \
      gcc-7 \
      g++-7 \
      gdb \
      git \
      gperf \
      help2man \
      llvm-9 \
      llvm-10 \
      llvm-11 \
      llvm-12 \
      llvm-13 \
      llvm-9-dev \
      llvm-10-dev \
      llvm-11-dev \
      llvm-12-dev \
      llvm-13-dev \
      llvm-14-dev \
      llvm-15-dev \
      llvm-16-dev \
      make \
      kmod \
      ninja-build \
      python3.8 \
      python3.8-dev \
      python3.8-distutils \
      python3-setuptools \
      python-is-python3 \
      rpm2cpio \
      vim \
      xz-utils \
      z3
# Remove conflicting automatically installed GCC versions
RUN apt-get remove -y cpp gcc g++ && \
# Install alternatives for all LLVM versions and for GCC 7
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-9 40 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-9 40 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-9 40 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-9 40 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-10 50 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-10 50 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-10 50 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-10 50 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-11 60 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-11 60 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-11 60 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-11 60 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-12 70 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-12 70 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-12 70 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-12 70 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-13 80 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-13 80 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-13 80 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-13 80 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-14 80 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-14 80 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-14 80 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-14 80 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-15 80 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-15 80 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-15 80 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-15 80 && \
    update-alternatives --install /usr/local/bin/llvm-config llvm-config /usr/bin/llvm-config-16 80 && \
    update-alternatives --install /usr/local/bin/clang clang /usr/bin/clang-16 80 && \
    update-alternatives --install /usr/local/bin/opt opt /usr/bin/opt-16 80 && \
    update-alternatives --install /usr/local/bin/llvm-link llvm-link /usr/bin/llvm-link-16 80 && \
    update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 100 --slave /usr/bin/g++ g++ /usr/bin/g++-7 && \
    update-alternatives --install /usr/bin/x86_64-linux-gnu-gcc x86_64-linux-gnu-gcc /usr/bin/x86_64-linux-gnu-gcc-7 100 \
                        --slave   /usr/bin/x86_64-linux-gnu-g++ x86_64-linux-gnu-g++ /usr/bin/x86_64-linux-gnu-g++-7
# Configure links for the clang-format checker script
RUN update-alternatives --install /usr/local/bin/clang-format clang-format /usr/bin/clang-format-8 40 && \
    ln -s /diffkemp/tools/check-clang-format.sh /usr/local/bin/check-clang-format
# Setup Python 3.8 and install pip packages
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.8 100
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.8
RUN python3 -m pip install \
      --upgrade pip \
      llvmcpy \
      pytest \
      pyyaml \
      --ignore-installed
RUN git clone https://github.com/viktormalik/rhel-kernel-get.git && \
    python3 -m pip install -r rhel-kernel-get/requirements.txt
# Copy neccessary scripts
COPY install.sh /install.sh
COPY switch-llvm.sh /switch-llvm.sh
RUN chmod +x /install.sh
RUN chmod +x /switch-llvm.sh
ENTRYPOINT /install.sh; /bin/bash
