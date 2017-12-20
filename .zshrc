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

t() {
    if tmux list-sessions 2&>1 > /dev/null; then
        sess_num=$(tmux list-sessions | grep -oP "^[0-9]:" | grep -o "[0-9]" | head -n 1)
        tmux attach-session -t $sess_num
    else
        tmux
    fi
}

# md2pdf() {
#     pandoc '$1.md' --latex-engine=xelatex --variable mainfont='Songti SC' -o '$1.pdf'
# }

docker_clean() {
    docker rm -v $(docker ps -a -q -f status=exited)
}

docker_clean_images() {
    docker rmi $(docker images -f "dangling=true" -q)
}

pyenv_install() {
    if [[ -z "$1" ]]; then
        echo 'Usage: $0 <version>'
    else
        wget http://mirrors.sohu.com/python/$1/Python-$1.tar.xz -P ~/.pyenv/cache/
        pyenv install $1
    fi
}

o() {
    if [[ -z "$1" ]]; then
        open .
    else
        open $1
    fi
}

tldr() {
    proxychains4 /usr/local/bin/tldr $@
}

rename_by_md5() {
    md5 -r * | sed -e 's/\([^ ]*\) \(.*\(\..*\)\)$/mv -v \2 \1\3/'
}

boring() {
    if [[ $((${RANDOM} % 2)) -eq 0 ]]; then
        CMD=cowsay
    else
        CMD=cowthink
    fi
    FMTS=($($CMD -l | tail -n +2 | xargs))
    RAND=$((${RANDOM} % ${#FMTS[@]} + 1))
    saikou --no-color | $CMD -f ${FMTS[$RAND]}
}

fkill() {
    PID=$(ps -axo pid,user,%cpu,%mem,comm | tail +2 | fzf --tac | awk '{print $1}')
    if [[ -z "$PID" ]]; then
        return 1
    fi

    if [[ -z "$1" ]]; then
        kill "$PID"
    else
        kill "$1" "$PID"
    fi
}

# config fasd
fcd() {
    DIRS=($(z -l $1 | grep '^[0-9]' | awk '{print $2}' | uniq))
    N=${#DIRS[@]} 
    if [[ $N -eq 1 ]]; then
        cd $DIRS
    elif [[ $N -gt 1 ]]; then
        cd $(printf '%s\n' "${DIRS[@]}" | fzf --tac)
    fi
}

# goto cd history
dh() {
    DIR=$(dirs -v | head -30 | fzf --tac | cut -f2)
    DIR=$(sed -e 's/[[:space:]]*$//' <<< ${DIR})
    if [[ ${#DIR[@]} -eq 1 || "${DIR[2]}" == "/" ]]; then
        DIR="${DIR/#\~/$HOME}"
    fi
    cd $DIR
}

c() {
    cat $1 | copy
}

dfs() {
    hdfs dfs -$*
}

port() {
    sudo lsof -i :$*
}

kill_port() {
    kill $(port $1 | awk '{print $2}' | tail -n 1)
}

scpr() {
    if [ -z "$PORT" ]; then
        PORT=22
    fi

    rsync -rvP --rsh="ssh -p $PORT" $@
}

reload_my_hhkb() {
    if [[ -z "$1" ]]; then
        echo "Usage: $0 <hex_file>"
    else
        sudo dfu-programmer atmega32u4 erase
        dfu-programmer atmega32u4 flash $1
        dfu-programmer atmega32u4 reset
    fi
}

ftp_upyun() {
    lftp -u $FTP_UPYUN_USER,$1 $FTP_UPYUN
}

mosh_to() {
    mosh --ssh="ssh -p $VPS_PORT" $1
}

ssh_proxy() {
    mosh_to loggerhead@$PROXY_VPS
}

ssh_www() {
    mosh_to fz@$WWW_VPS
}

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

source "${HOME}/.zsh-aliases"
