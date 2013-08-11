# Bash settings.
#
# First, set values I want for all shells, even non-interactive ones.
#

# Paths.
#export PATH=...
#export LD_LIBRARY_PATH=...

# Editors.
export EDITOR="emacs -nw -q"

# Misc.
# ...

# Aliases.
if [ -f "${HOME}/.bash_aliases" ]; then
  source "${HOME}/.bash_aliases"
fi

# Machine local settings.
if [ -f "${HOME}/.bash_local" ]; then
  source "${HOME}/.bash_local"
fi

#
# If not running interactively, bail out now.
#
[[ "$-" != *i* ]] && return
# Can also do:
# [ -z "$PS1" ] && return

# History.

# Append rather than overwrite.
shopt -s histappend

# Ignore both empty lines and duplicate commands in the history.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoreboth

# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'


# Check the window size after each command to set LINES and COLUMNS.
shopt -s checkwinsize

# Ignore ctrl-s.
stty ixany
stty ixoff -ixon

# Decide whether to use colors.
case "$TERM" in
    xterm*) color_prompt=yes;;
    rxvt*) color_prompt=yes;;
esac

# Use the colored prompt.
if [ "$color_prompt" = yes ]; then
    # White text on blue background.
    PS1="\[\033[0;37;44m\]\h \w \! \$\[\033[0m\] "
else
    PS1="\h \w \! \$ "
fi
unset color_prompt
