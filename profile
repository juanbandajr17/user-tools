# Setting Prompt
# If id command returns zero, you’ve root access.
if [ $(id -u) -eq 0 ];
then # you are root, set red colour prompt
  PS1="\\[$(tput setaf 1)\\]:\\w #\\[$(tput sgr0)\\] "
else # normal
  PS1="[\\w] $ "
fi

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
