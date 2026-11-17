# .zprofile

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/password-store"
export GNUPGHOME="${XDG_CONFIG_HOME}/gnupg"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export ANDROID_USER_HOME="${XDG_DATA_HOME}/android"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME}/npm"
export W3M_DIR="${XDG_STATE_HOME}/w3m"
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
export MYPY_CACHE_DIR="${XDG_CACHE_HOME}/mypy"
export JUPYTER_CONFIG_DIR="${XDG_CONFIG_HOME}/jupyter"
export IPYTHONDIR="${XDG_CONFIG_HOME}/ipython"

path=("${HOME}/.local/bin" $path)
export PATH

export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="footclient"
export BROWSER="firefox"
export MANPAGER="nvim +Man!"

export GTK_CSD=0
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_ENABLE_HIGHDPI_SCALING=1
export MOZ_ENABLE_WAYLAND=1
export LIBVIRT_DEFAULT_URI='qemu:///system'
export CLAUDE_API_USAGE_KEY="$(pass claude-api-usage)"
