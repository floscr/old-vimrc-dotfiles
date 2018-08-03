# Set architecture flags
export ARCHFLAGS="-arch x86_64"

# Brew
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/sbin:$PATH

# PKG Config
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/

# Ruby
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH
export PATH=$HOME/.gem/bin:$PATH

# Mysql
export PATH=/usr/local/mysql/bin:$PATH

# Custom binaries
export PATH=$PATH:$HOME/.dotfiles/bin

# Android Tools
export ANDROID_TOOLS="$HOME/Library/Android/sdk/tools"
export ANDROID_PLATFORM_TOOLS="$HOME/Library/Android/sdk/platform-tools"
export PATH=$PATH:$ANDROID_TOOLS:$ANDROID_PLATFORM_TOOLS

# PHP & Composer
export PATH="/usr/local/opt/php70/bin:$PATH"
export PATH=~/.composer/vendor/bin:$PATH

# Doom
export PATH=$PATH:~/.emacs.d/bin

# Haskell
export PATH=$HOME/.local/bin:$PATH

# Rust
export PATH=$HOME/.cargo/bin:$PATH

# RVM
export PATH=$PATH:$HOME/.rvm/bin
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"

# Go
export GOPATH=$HOME/golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

