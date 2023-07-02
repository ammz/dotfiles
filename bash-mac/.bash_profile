# -*- mode: shell-script -*-
#
# Cualquier configuración que añadamos a .bash_profile no será
# efectiva hasta que salgamos de la cuenta y volvamos a logarnos, si
# hacemos cambios en este fichero y queremos verlos sin salir de la
# cuenta podemos usar el comando source, el cual ejecuta el contenido
# del fichero que le digamos:
#
# $ source .bash_profile
#
# Alternativamente al comando source está el comando punto (.), con lo
# que el contenido de .bash_profile también se podría haber ejecutado
# así:
#
# $ . .bash_profile
#
# Bash permite usar dos nombres alternativos para .bash_profile por
# razones de compatibilidad histórica: .bash_login, nombre derivado
# del fichero .login del C Shell, y .profile nombre usado por el
# Bourne Shell y el Korn Shell. En cualquier caso, sólo uno de estos
# ficheros será ejecutado. Si .bash_profile existe los demás serán
# ignorados, si no Bash comprobará si existe .bash_login y, sólo si
# éste tampoco existe, comprueba si existe .profile. La razón por la
# que se eligió este orden de búsqueda es que podemos almacenar en
# .profile opciones propias del Bourne Shell, y añadir opciones
# exclusivas de Bash en el fichero .bash_profile seguido del comando
# source .profile para que Bash también cargue las opciones del
# fichero .profile.
#
# .bash_profile se ejecuta sólo al logarnos, si abrimos otro shell
# (ejecutando bash o su) desde la línea de comandos de Bash lo que se
# intenta ejecutar es el contenido de .bashrc. Si .bashrc no existe no
# se ejecutan configuraciones adicionales al abrir un nuevo
# shell. Este esquema nos permite separar configuraciones que se hacen
# una sola vez, al logarnos, de configuraciones que se cambian cada
# vez que se abre un nuevo shell. Si hay configuraciones en .bashrc
# que también queremos ejecutar al logarnos podemos poner source
# .bashrc dentro del fichero .bash_profile.

export CLICOLOR=1
export TERM=xterm-color
export LESSCHARSET=utf-8
export LC_ALL=es_ES.UTF-8
export LANG=es_ES.UTF-8

alias ls='ls -G'
alias ll='ls -lah'
alias la='ls -ah'
# alias tree=tree.sh
alias dep='php vendor/bin/dep'

# alias vocean='cd /Users/ammz/proyectos/maquinas/ocean'
# alias vgandalf='cd /Users/ammz/proyectos/maquinas/gandalf'

export GREP_OPTIONS='--color=auto'

# OS X Leopard+ tiene las rutas por defecto definidas en "/etc/paths" y
# "/etc/manpaths". Además permite añadir nuevas rutas si creamos un nuevo
# fichero en "/etc/paths.d" con éstas definidas en él. Se puede ver el fichero
# /etc/paths.d/TeX como ejemplo. Debe ser instalado por defecto por MacTeX.

export PATH=$HOME/.emacs.d/bin:$HOME/bin:/usr/local/sbin:$PATH

# Añade PATH para los ejecutables de stack
export PATH="${HOME}/.local/bin:${PATH}"

# Si usamos "ConTeXt Suite" debemos desmarcar la siguiente línea. Hay alguna
# incompatibilidad con las rutas. Si queremos utilizar latex habrá que marcarla
# y volver a arrancar emacs.
# export PATH=/Users/ammz/context-suite/tex/texmf-osx-64/bin:$PATH
# export PATH=/Users/ammz/context-lmtx/tex/texmf-osx-64/bin:$PATH

export INFOPATH=/usr/local/share/info:/usr/local/share/info/emacs:/usr/share/info

# export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
# export WORKON_HOME=$HOME/.local/share/virtualenvs
# export PROJECT_HOME=$HOME/dev
# source /usr/local/bin/virtualenvwrapper.sh

# if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
# if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# export PATH=~/Library/Python/3.7/bin:$PATH

# Para activar los ficheros ejecutables de mysql
export PATH=/usr/local/mysql/bin:$PATH

# Lenguaje go
export PATH=$PATH:/usr/local/go/bin

export DISPLAY=:0.0

export OSFONTDIR="/Library/Fonts//;/System/Library/Fonts//;$HOME/Library/Fonts//;$HOME/context/tex/texmf/fonts//;/usr/local/texlive/2023/texmf-dist/fonts//;/Applications/Microsoft Word.app/Contents/Resources/Fonts//"
export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.11/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

alias em="/Applications/Emacs.app/Contents/MacOS/Emacs $@ &"
alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a em -n $@"
alias sudoec="sudo -e"
export EDITOR=emacsclient
export SUDO_EDITOR=sudoec
export EDITOR_ALTERNATIVE=emacs


alias showhidden="defaults write com.apple.finder AppleShowAllFiles TRUE && killall Finder"
alias hidehidden="defaults write com.apple.finder AppleShowAllFiles FALSE && killall Finder"

# Recomendado por la documentación de brew para "bash-completion"
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

RED="\[\033[0;31m\]"
YELLOW="\[\033[0;33m\]"
GREEN="\[\033[0;32m\]"
WHITE="\[\e[0m\]"

# PS1='\[\033[0;32m\]\w`__git_ps1`\n\[\e[0m\]\h \$ '
# PS1='\[\033[0;32m\]\w\n\[\e[0m\]\h \$ '

# recomendado por la documentación de brew para "bash-git-prompt"
if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
    source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
  fi

# Recomendado por la documentación de RVM
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"


# export JAVA_HOME="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"


# export DOCKER_HOST=tcp://192.168.59.103:2376
# export DOCKER_CERT_PATH=/Users/ammz/.boot2docker/certs/boot2docker-vm
# export DOCKER_TLS_VERIFY=1


# Necesario para python instalado por brew
export PATH="/usr/local/opt/python/libexec/bin:$PATH"

# test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# [[ -f ~/.bashrc ]] && source ~/.bashrc

export CPATH=/usr/local/include:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/
# export CPATH=/Library/Developer/CommandLineTools/usr/include/c++/v1
# export CPATH="/usr/local/include"
# export CPLUS_INCLUDE_PATH="/usr/local/include"
# export C_INCLUDE_PATH="/usr/local/include"
# export LIBRARY_PATH="/usr/local/lib"

# source ~/.bashrc

export PATH="/usr/local/opt/llvm/bin:$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/ammz/opt/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/ammz/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/ammz/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/ammz/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# PATH="/usr/local/opt/php@7.2/bin:$PATH"
# PATH="/usr/local/opt/php@7.2/sbin:$PATH"
# export LDFLAGS="-L/usr/local/opt/php@7.2/lib"
# export CPPFLAGS="-I/usr/local/opt/php@7.2/include"
#[[ -f ~/.bashrc ]] && source ~/.bashrc # ghcup-env
# [[ -f ~/.bashrc ]] && source ~/.bashrc # ghcup-env
