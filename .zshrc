# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
plugins=(
    z rust brew command-not-found
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

alias pdl='proxychains4 aria2c -c'
alias vscode='/usr/local/bin/Electron'
alias tor='proxychains4 tor'
alias nosetests3='python3 -m "nose"'
alias la="ls -a"
alias vimconfig="vim ~/.vimrc"
alias tmuxconfig="vim ~/.tmux.conf"
alias tmuxkeyconfig="vim ~/.tmux/bind-key.conf"
alias nginxconfig="sudo vim /usr/local/nginx/conf/nginx.conf"
alias zshconfig="vim ~/.zshrc"

alias nginxreload="sudo /usr/local/nginx/sbin/nginx -s reload"
alias zshreload="source ~/.zshrc"
alias tmuxreload="tmux source ~/.tmux.conf"

alias h="history | tail -n 30"
alias allownetwork="sudo codesign --force --sign - "

alias notebook="jupyter notebook &> /dev/null &"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias subl="open -a Sublime\ Text"
alias sublc="completion daemon &> /dev/null &"
alias ifind="mdfind -onlyin $PWD "
alias remove="/bin/rm"
alias cmake_clean="rm -rf CMakeCache.txt CMakeFiles/ Makefile cmake_install.cmake"
alias mongodbd="sudo mongod --fork --logpath /tmp/mongodb.log"
alias stopmongodb="sudo pkill mongod"
alias cask="brew cask"
alias vim="mvim -v"
alias irc="weechat"
alias wireshark="sudo chbpf && /usr/local/bin/wireshark"
alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'
alias py='pypy'
alias ppython='ptpython'
alias objdump='gobjdump'
alias copy='pbcopy'
alias paste='pbpaste'
alias erlang='kjell'
alias ftp='lftp'
alias httpd='echo_server.py'
alias sync_imgs="rsync -av $IMG_DIR $USER@$VPS::sync"

alias t="tmux -2"
alias p="parallel --pipe -k"
alias r="trash-put"
alias s="rg"
alias f="ag -l"
alias j="fcd"
alias lf="ls | fzf --tac"

# md2pdf() {
#     pandoc '$1.md' --latex-engine=xelatex --variable mainfont='Songti SC' -o '$1.pdf'
# }

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
    DIRS=($(z -l $1 | sort -r | awk '{print $2}' | uniq))
    if [[ ${#DIRS[@]} -le 1 ]]; then
        cd $DIRS
    else
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
    if [[ -z $1 ]]; then
        cat | copy
    else
        cat $1 | copy
    fi
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

ftp_upyun() {
    lftp -u $FTP_UPYUN_USER,$1 $FTP_UPYUN
}

ssh_lab() {
    mosh --ssh="ssh -p $VPS_PORT" $USER@$LAB
}

ssh_lab2() {
    ssh $USER@$LAB2
}

ssh_lab_dev() {
    mosh $USER@$LAB_DEV
}


ssh_proxy() {
    mosh --ssh="ssh -p $VPS_PORT" $USER@$SS_PROXY
}

ssh_vps() {
    mosh --ssh="ssh -p $VPS_PORT" $USER@$VPS
}

if [[ -n ${INSIDE_EMACS} ]]; then
    # This shell runs inside an Emacs *shell*/*term* buffer.
    unsetopt zle
fi

export ZSH_PLUGINS_ALIAS_TIPS_TEXT="Tip: "
export ENHANCD_DISABLE_DOT=1
export ENHANCD_DISABLE_HYPHEN=1
export ENHANCD_COMMAND=ecd

source $ZSH/oh-my-zsh.sh
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

    zgen save
fi

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=green'
eval "$(pyenv init -)"
