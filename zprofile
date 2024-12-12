# .zprofile

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/password-store"
export GNUPGHOME="${XDG_CONFIG_HOME}/gnupg"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

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
