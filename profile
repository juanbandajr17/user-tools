[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Setting Prompt
# If id command returns zero, you’ve root access.
if [ $(id -u) -eq 0 ];
then # you are root, set red colour prompt
  PS1="\\[$(tput setaf 1)\\]:\\w #\\[$(tput sgr0)\\] "
else # normal
  PS1="[\\w] $ "
fi

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias excel="open -b com.microsoft.Excel"
alias maclogout="osascript -e 'tell application \"System Events\" to log out'"

# Use correct version of easy_install
export PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
