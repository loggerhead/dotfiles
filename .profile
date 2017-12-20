source ~/.my_secret
# ONLY work on Linux, that is, not work on OS X
# export LD_LIBRARY_PATH=$HADOOP_HOME/lib/native:$LD_LIBRARY_PATH
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TERM="xterm-256color"
export EDITOR=vim
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export PYTHONIOENCODING="UTF-8"

export FTP_UPYUN="v0.ftp.upyun.com"
export MACKUP_DIR="$HOME/Nutstore/"
export IMG_DIR="$HOME/.DS_Store/imgs"
export NIU_GE_SYNC_DIR="$IMG_DIR"
export BLOG_WORKDIR="$HOME/codes/projects/blog-deployer/pelican-config"

export XDG_CONFIG_HOME=$HOME/.config

export HOMEBREW_PREFIX=/usr/local/Cellar
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export HOMEBREW_BOTTLE_DOMAIN='https://mirrors.ustc.edu.cn/homebrew-bottles'

export HADOOP_HOME=$(brew --prefix hadoop)
export YARN_HOME=$HADOOP_HOME
export HBASE_HOME=$(brew --prefix hbase)
export THRIFT_HOME=$(brew --prefix thrift)
export ZOOKEEPER_HOME=$(brew --prefix zookeeper)
export KAFKA_HOME=$(brew --prefix kafka)

export GOPATH=$HOME/go

export ANDROID_HOME=$HOME/Library/Android/sdk
export POSTGRES_PATH=/Applications/Postgres93.app/Contents/MacOS/bin

export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home"
export CLASSPATH=".:./weka.jar:/usr/local/lib:/Applications/weka-3-6-13/weka.jar:$KAFKA_HOME/libs:$THRIFT_HOME/lib:$HBASE_HOME/lib:$HADOOP_HOME/lib:$CLASSPATH"
export GENERATED_JAVAH=$JAVA_HOME

export LIBFM_PATH="$HOME/Documents/codes/projects/graduation/final/libfm/bin"

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/codes/shell:$HOME/codes/myscript:$HOME/.cargo/bin:/usr/local/bin:/usr/local/sbin:$JAVA_HOME:$POSTGRES_PATH:$HBASE_HOME/bin:$HADOOP_HOME/bin:$HADOOP_HOME/sbin:$PATH

export RUST_BACKTRACE=1
export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src
export CARGO_HOME=$HOME/.cargo
