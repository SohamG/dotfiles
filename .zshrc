# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle :compinstall filename '/home/sohamg/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install

export VI_MODE_SET_CURSOR=true
bindkey -M viins '\e.' insert-last-word

setopt no_nullglob
setopt no_nomatch
DRAC_BG=(40 42 54)
DRAC_PINK=(255 121 198)
DRAC_YELLOW=(241 250 140)
DRAC_RED=(255 85 85)
DRAC_COMMENT=(98 114 164)
DRAC_CURRENT=(68 71 90)

#SEP='%{\uE0B0 %}'
SEP='%{%G\uE0B0%}'

SET_BG_COLOR () {
    echo -en "%{%G\x1b[48;2;${1};${2};${3}m%}"
}
SET_FG_COLOR () {
  echo -en "%{%G\x1b[38;2;${1};${2};${3}m%}"
}
RESET_FG(){
  echo -en "%{%G\x1b[0m%}"
}

FG_COLOR () {
    case $PWD in
        /usr*|/opt*)
          SET_FG_COLOR ${DRAC_YELLOW[@]}
            ;;
        /etc*|/root*)
          SET_FG_COLOR ${DRAC_RED[@]}
            ;;
          *)
            SET_FG_COLOR ${DRAC_BG[@]}
    esac
}
PRINT_SEP(){
  SET_FG_COLOR $@
  echo -en $SEP
}
PRINT_DIR(){
  PWD2=$PWD
  case $PWD2 in
    $HOME*)
#      echo "%{%G🏠%}"
      PWD2=${PWD2##$HOME}
  esac 
   echo "$PWD2" | sed 's%/%%' |awk 'BEGIN{FS="/"} {i=(NF<3)?1:int(NF-2); for(;i<=NF;i++) {
   print $i "%{%G%}"
 }}' # cmon awk dont be dumb
}

# Use modern completion system
make_prompt(){
  SET_BG_COLOR ${DRAC_PINK[@]} # dracula colors
  FG_COLOR
  PRINT_DIR
  RESET_FG
  PRINT_SEP ${DRAC_PINK[@]}
  RESET_FG
}

# PS1='$(make_prompt | tr -d "\n")'
### SETUP ZSH OPTIONS ###
setopt histignorealldups sharehistory menu_complete
###
### HISTORY FILE OPTIONS ###
# Keep 50000 lines of history within the shell and save it to ~/.zsh_history:

#autoload -Uz compinit
#compinit
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
# path+="/home/sohamg/.emacs.d/bin"
# setxkbmap -option caps:ctrl_modifier,shift:both_capslock
#Emacs vterm

# vterm_printf(){
#     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
#         # Tell tmux to pass the escape sequences through
#         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
#     elif [ "${TERM%%-*}" = "screen" ]; then
#         # GNU screen (screen, screen-256color, screen-256color-bce)
#         printf "\eP\e]%s\007\e\\" "$1"
#     else
#         printf "\e]%s\e\\" "$1"
#     fi
# }

# if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
#     alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
# fi
eval "$(direnv hook zsh)"
mysync() {
	rsync -e 'ssh -p 6969' $@
}

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
