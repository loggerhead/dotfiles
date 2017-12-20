# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
plugins=(
    z brew command-not-found
    cabal docker gem git
    git-extras httpie npm redis-cli
    pip extract python zsh-autosuggestions
)

ZSH_TMUX_AUTOSTART=true
# ZSH_TMUX_AUTOCONNECT=true
ZSH_TMUX_AUTOQUIT=true

eval "$(fasd --init zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install)" >> "/dev/null" 2>&1
#[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

PROMPT_TITLE='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
export PROMPT_COMMAND="${PROMPT_COMMAND} ${PROMPT_TITLE}; "

bindkey -e
bindkey '^[[1;9C' forward-word
bindkey '^[[1;9D' backward-word
bindkey "[D" backward-word
bindkey "[C" forward-word

stty -ixon
ZSH_THEME="robbyrussell"

DISABLE_AUTO_UPDATE="true"
DISABLE_UPDATE_PROMPT="true"
DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"


if [[ -n ${INSIDE_EMACS} ]]; then
    # This shell runs inside an Emacs *shell*/*term* buffer.
    unsetopt zle
fi

export ZSH_PLUGINS_ALIAS_TIPS_TEXT="Tip: "
export ENHANCD_DISABLE_DOT=1
export ENHANCD_DISABLE_HYPHEN=1
export ENHANCD_COMMAND=ecd

source "${HOME}/.zsh/zgen/zgen.zsh"

if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    zgen loadall <<EOPLUGINS
        djui/alias-tips
        jimmijj/zsh-syntax-highlighting
        voronkovich/gitignore.plugin.zsh
        rimraf/k
        b4b4r07/enhancd
EOPLUGINS

    zgen load tarruda/zsh-autosuggestions
    zgen load zsh-users/zsh-syntax-highlighting

    zgen save
fi

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=green'
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

# added by travis gem
[ -f /Users/fz/.travis/travis.sh ] && source /Users/fz/.travis/travis.sh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source "${HOME}/.zsh-functions"
source "${HOME}/.zsh-aliases"
