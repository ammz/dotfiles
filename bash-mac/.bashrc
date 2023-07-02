export CLICOLOR=1
export TERM=xterm-color
export LESSCHARSET=utf-8
export LC_ALL=es_ES.UTF-8
export LANG=es_ES.UTF-8

alias ls='ls -G'
alias ll='ls -la'
alias la='ls -a'
alias tree=tree.sh

export GREP_OPTIONS='--color=auto'

# recomendado por la documentación de brew para "bash-git-prompt"
if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
    source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
  fi

# eval "$(pipenv --completion)"
# [ -f "/Users/ammz/.ghcup/env" ] && source "/Users/ammz/.ghcup/env" # ghcup-env
