# begin .zprofile
if [[ -z "$SHIFTER_RUNTIME" ]]
then
  [ -e $HOME/.dbgdot ] && echo "entering .zprofile" >&2


# User-specific settings
  if [ -e $HOME/.zprofile.ext ]; then
        . $HOME/.zprofile.ext
  fi

  [ -e $HOME/.dbgdot ] && echo "exiting .zprofile" >&2
fi
# end .zprofile
