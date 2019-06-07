# begin .zshenv
if [[ -z "$SHIFTER_RUNTIME" ]]
then
  [ -e $HOME/.dbgdot ] && echo "entering .zshenv" >&2
  
  if [ -z "$NERSC_HOST" ]; then export NERSC_HOST=`cat /etc/clustername`;fi

#  [[ -o login ]] && setopt SH_WORD_SPLIT
  # Interative shell variables and settings
  if [[ -z $PROFILEREAD ]]; then
    MANPATH=/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man;export MANPATH
    case "$NERSC_HOST" in
      cori|edison|gerty|alva|carl)
        export MANPATH="/usr/common/software/man:/usr/common/mss/man:/usr/common/nsg/man:"$MANPATH
        export PATH="/usr/common/software/bin:/usr/common/mss/bin:/usr/common/nsg/bin:"$PATH
      ;;
      *)
        export MANPATH="/usr/common/usg/man:/usr/common/mss/man:/usr/common/nsg/man:"$MANPATH
        export PATH="/usr/common/usg/bin:/usr/common/mss/bin:/usr/common/nsg/bin:"$PATH
      ;;
    esac
  fi
  
  alias chsh='echo "Use NIM (https://nim.nersc.gov/) to change your shell"'
  alias passwd='echo "Use NIM (https://nim.nersc.gov/) to change your password"'
  
  #get module function defined
  if [ "$NERSC_HOST" = "edison" -o "$NERSC_HOST" = "cori" -o "$NERSC_HOST" = "alva" ];then
    #if [ -z "`type module 2>/dev/null`" ]; then
        . /opt/modules/default/etc/modules.sh
    #fi
  fi
  
  # User-specific settings
  
  if [ -e $HOME/.zshenv.ext ]; then
  	. $HOME/.zshenv.ext
  fi
  
  [ -e $HOME/.dbgdot ] && echo "exiting .zshenv" >&2
fi
# end .zshenv
