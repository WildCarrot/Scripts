# -*- mode: sh; -*-
#
# Aliases for bash.
#

alias ll='ls -lF --color=tty'
alias up='cd ..'
alias locat='locate'
alias scr='screen'
alias whence='type -a'
alias backup='rsync -avz'
alias start='explorer'

function open() {
    os=`uname -s`
    if [[ "$os" =~ "CYGWIN_NT" ]]; then `start $*`; fi;
    if [[ "$os" =~ "Darwin" ]]; then `open $*`; fi;
    if [[ "$os" =~ "Linux" ]]; then `nautilus $*`; fi;
}

function upper() {
    echo $1 | python -c "import sys; print sys.stdin.readline().upper()[0:-1]"
}
function dec2hex() {
    num=`upper $1`
    echo "obase=16;" $num | bc
}
function dec2bin() {
    num=`upper $1`
    echo "obase=2;" $num | bc
}
function hex2dec() {
    num=`upper $1`
    echo "ibase=16;" $num | bc
}
function hex2bin() {
    num=`upper $1`
    echo "ibase=16;obase=2;" $num | bc
}

# ^[ is an ESC key, not control-[
# \033 = ESC   \007 = bell   but the octal doesn't always work
function settitle() {
    #echo -ne "\e]2;$@\a\e]1;$@\a";
    echo -n "]0;${1}";
}


# Revision Control
#
alias lsco='git status -s -uno'
alias lsv='git status -s -u'
alias lsbr='git branch -v'
alias lshist='git log --pretty=format:"%cr -- Commit %h by %cn -- %s" -n 10'
alias unco='git reset HEAD --'
alias srcco='git add'
alias srcci='git commit -m'
alias srcupd='git pull'
alias pdiff='git diff --cached'

function lscoci() {
    git status -s -uno $* | awk '{print $2}'
}

function uncokeep() {
    cp -i $1 $1.keep;
    git reset HEAD --;
}

function setv() {
    export SRC_ROOT=$1;
}

function src() {
    cd $SRC_ROOT/$*;
}

function gitme() {
    git config user.name "Laura Beegle"
    git config user.email laura@beegle.org
}

# Build commands
#
alias mmake='make'
alias utmake='make tests'

# Misc
#
alias foreach="echo 'for name in word; do list; done'"
alias termcolors="echo 'fg=#f0d030 cr=#f00000 bg=#000000'"

# ------ Revision control commands for other systems ------
#alias lsco='p4 opened'
#alias lsco    'ct lsco -cvi -me -fmt "%-40.40En (%Rf) \t %Nc \n"'
#alias lsco    'cvs -q update -l'
#alias lscog='git status -uno -s'
#alias lscogb='git diff --cached --name-status `git remote`/top'

#alias lscor='p4 opened'
#alias lscor    'ct lsco -r -cview -me -fmt "%-60En (%Rf) %Nc \n"'
#alias lscor	'cvs -q update'

#alias lscoci    'cat tmp | awk -F# '"'"'{print $1}'"'"
#alias lscoci='p4 opened | awk -F# '"'"'{print $1}'"'"' | awk -F/ '"'"'{for (i=7; i <= NF; i++) {printf("%s", $i); if (i != NF) {printf("%s", FS); }} print "";}'"'"
#alias lscoci	'ct lsco -cvi -me -fmt "%En\n"'
#alias lscoci	'cvs -q update -l \!* | awk '"'{print "'$2'"}'"
#alias lscocig="git status -uno -s | awk '{print \$2}'"

#alias lscocir='echo "Figure out in P4"'
#alias lscocir	'ct lsco -cvi -me -r -fmt "%En\n"'
#alias lscocir	'cvs -q update \!* | awk '"'{print "'$2'"}'"

# alias lsbr='echo "Figure out in P4"'
# alias lsbrg='git branch -v'

#alias unco='p4 revert'
#alias unco	'ct unco -rm \!*'
#alias unco	'rm \!*;cvs -q update \!*' # This one is untested!
#alias uncog='git reset HEAD --'

# function uncokeep() {
#     cp -i $1 $1.keep;
#     p4 revert $1;
# }
#alias uncokeep='cp -i \!* \!*.keep;p4 revert \!*'
#alias uncokeep 'ct unco -keep \!*'
#alias uncokeep 'cp -i \!* \!*.keep;cvs -q update \!*' #untested

#alias uncoempty='if ( `pdiff \!* | grep -v "==== //depot/prod" | wc -l` == 0) p4 revert \!*'

#alias lshist='p4 changes -m 10'
#alias lshist	'ct lshistory -last 10 -fmt "%Xn %Sd %u %Nc\n"'
#alias lshist	'echo "Figure out in CVS"'
#alias lshistg='git log --pretty=format:"%cr -- Commit %h by %cn -- %s" -n 10'

#alias lsv='p4lsv'
#alias lsv	'ct ls -view -short \!* | grep -v \~'
#alias lsv	'cvs -q update -l \!* | grep "?"'

#alias lsvr='p4lsvr'
#alias lsvr	'ct ls -view -short -r \!* | grep -v \~'
#alias lsvr	'cvs -q update \!* | grep "?"'

#alias srcupd='p4 sync | grep -v updating'
#alias srcupd	'ct update -log /dev/null'
#alias srcupd	'cvs update'
#alias srcupdg='git remote update -p;git branch -f before_rebase_`date +"%Y_%m_%d"`;git rebase `git remote`/top'

#alias srcco='p4 edit'
#alias srcco    'ct co -nc'
#alias srcco    'echo'
#alias srccog='git add'

# alias srcci='echo "figure out in p4"'
# alias srccig='git commit -uno -m'

#alias pdiff='p4 diff'
#alias pdiff	'ct diff -pre'
#alias pdiff	'cvs diff'
#alias pdiffg='git diff --cached'

#alias cocomment='echo \!* >> ~/$P4ROOT/COCOMMENTS'
#alias cocomment      'ct chevent -replace -c'

#alias coaddcomment='echo "Figure out in P4"'
#alias ctaddcomment   'ct chevent -c'

#alias getprever='echo "Figure out in P4"'
#alias getprever   'ct desc -fmt "\!*@@%PVn\n" \!*'
#alias getprever "cvs status "'\!*'" | head -4 | tail -1 | awk '{print "'$3'"}'"

#alias getprev='p4 print -q file#4 > file.prev'
#alias getprev	 'ct get -to \!*.prev `getprever \!*`'
#alias getprev   'echo "Figure out in CVS"'

#alias changenum='p4 changes -m1'
