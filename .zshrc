# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


p10_theme="$HOME/.nix-profile/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"

source $p10_theme
source ~/.p10k.zsh 

#To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export AGNOSTER_DISABLE_CONTEXT="1"
zstyle ':completion:*' completer _complete _ignored _approximate
zstyle :compinstall filename '/home/sohamg/.zshrc'

fpath+=~/.zfunc
autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
bindkey -v
# End of lines configured by zsh-newuser-install

export VI_MODE_SET_CURSOR="true";


bindkey -M viins '\e.' insert-last-word
bindkey -v "^?" backward-delete-char
bindkey -v "^[[3~" delete-char


setopt histignorealldups sharehistory menu_complete
HISTSIZE=50000
SAVEHIST=50000
HISTFILE=~/.zsh_history

zstyle ':completion:*' menu select
if [[ -f ~/.zsh_aliases ]]; then
    . ~/.zsh_aliases
fi

GAMEMODERUNEXEC="env __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia"
export GAMEMODERUNEXEC

path+="/home/sohamg/.local/scripts"

nvrun(){
    # env __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia $@
    prime-run $@
}
export KUBECONFIG=/home/sohamg/.kube/config
eval "$(direnv hook zsh)"
alias http="python3 -m http.server 8080 --directory"

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

fcd() {
    if [ -z $1 ]; then
        echo "Usage: $0 path"
    else
        cd $1/$(ls $1 | fzf)
    fi
}

take() {
    if [ ! -z $1 ]; then
        mkdir -p $1 && cd $1
    fi
}
cleantex() {
    rm -rf *.aux *.log *.fls 
}


shlvl_prompt() {
    if test "$SHLVL" = "1"; then
        echo ""
    else
        echo "$SHLVL"
    fi
}

export RPROMPT="%{$(shlvl_prompt)%}"
export SHELL=$(realpath $(which zsh))


alias remacs="systemctl restart --user myemacs"

alias spdf="sudo podman run --rm -it --privileged -p 8080:8080 stirlingtools/stirling-pdf:latest"

export LC_ALL=en_US.UTF-8

isLUHNValid() {
  local pan="$1"
  local panlen="${#pan}"
  local sum=0

  for ((i = panlen - 1; i >= 0; i--)); do
    local digit="${pan:$i:1}"
    if (((panlen-i) % 2 == 0)); then
       #even
       ((digit*=2))
       ((${#digit} == 2)) && digit=$((${digit:0:1}+${digit:1:1}))
    fi
    ((sum+=digit))
  done

  ((sum % 10 == 0))
}

alias -g CLOCK="--builders \"ssh://root@172.29.0.26 x86_64-linux /root/id_acm 12 12 big-parallel\""
alias -g L="| less"
alias -g G="| grep -i"

alias s="sudo systemctl"
alias us="systemctl --user"
export NIXPKGS_ALLOW_UNFREE=1

alias k="kubectl"
ka() {
    kubectl $@ -A
}
ks() {
    kubectl $@ -n kube-system
}

frfr() {
    if test "$#" -eq 0; then
	echo "usage: $0 <binary in path>"
    else
	realpath $(which $1)
    fi
}
export BROWSER=app.zen_browser.zen
alias sudo="run0"

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /nix/store/dk1psxq41wxki08ry62i61pvjpgl8bb9-tanka-0.32.0/bin/tk tk

PATH="/home/sohamg/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/sohamg/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/sohamg/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/sohamg/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/sohamg/perl5"; export PERL_MM_OPT;
