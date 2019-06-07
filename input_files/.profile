# begin .profile
if [[ -z "$SHIFTER_RUNTIME" ]]
then
  [ -e $HOME/.dbgdot ] && echo "entering .profile/.bash_profile" >&2

# File system settings

# Default umask:  user rw, group r, other none
  umask 007

# Note that datatran and scigate still needs complete /etc/profile.d support
  my_host=`cat /etc/clustername`
  if [ "$my_host" == "scigate" ]
  then
    export NERSC_HOST=scigate
  fi
  case "$NERSC_HOST" in
    datatran|scigate)
      export MANPATH="/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man"
      export BSCRATCH="/global/projectb/scratch/${USER}"
    ;;
    *)
    ;;
  esac

# Note that SCRATCH already set to GSCRATCH or BSCRATCH in /etc/csh.login on Genepool
#for JGI users on Cori, added on 11/8/2018
if [[ $(/usr/bin/groups) == *genome* ]]; then 
    export BSCRATCH=/global/projectb/scratch/$USER
fi

# Define TMPDIR
# For Genepool and Denovo TMPDIR should be set by SGE if possible;
# otherwise, use $SCRATCH
case "$NERSC_HOST" in
  denovo)
    if [ -n "$BSCRATCH" ]; then
      export SCRATCH=$BSCRATCH
    fi
    if [ -z "$TMPDIR" ]
    then
      export TMPDIR=/scratch/${USER}
      if [ ! -d $TMPDIR ]
      then
        mkdir $TMPDIR
      fi
    fi
  ;;
  edison|cori)
    export TMPDIR=/tmp
  ;;
  *)
  ;;
esac

# Software environmnent settings

# Common Unix commands preempted by NIM
  alias chsh='echo "Use NIM (https://nim.nersc.gov/) to change your shell"'
  alias passwd='echo "Use NIM (https://nim.nersc.gov/) to change your password"'

# Settings for NERSC-provided commands
  case "$NERSC_HOST" in
    cori|edison|gerty|alva)
      if [ -n "$MODULESHOME" ]; then
          module load gcc
      fi
      export MANPATH="/usr/common/software/man:/usr/common/mss/man:/usr/common/nsg/man:"$MANPATH
      export PATH="/usr/common/software/bin:/usr/common/mss/bin:/usr/common/nsg/bin:"$PATH
    ;;
    *)
      export MANPATH="/usr/common/usg/man:/usr/common/mss/man:/usr/common/nsg/man:"$MANPATH
      export PATH="/usr/common/usg/bin:/usr/common/mss/bin:/usr/common/nsg/bin:"$PATH
    ;;
    esac

# Load USG default modules
  case "$NERSC_HOST" in
    genepool|denovo)
      if [ ! -e ${HOME}/.no_default_modules ]
      then
        if [ -n "${MODULESHOME}" ]
        then
          module load usg-default-modules
        fi
      fi
    ;;
    *)
    ;;
  esac

# Settings for Intel compilers and tools
  if [ -z "$INTEL_LICENSE_FILE" ]; then
    export INTEL_LICENSE_FILE=28518@crayintel.licenses.nersc.gov:28518@intel.licenses.nersc.gov
  fi

# Host-specific settings

  case "$NERSC_HOST" in
    cori)
      export MAN_POSIXLY_CORRECT=1
      export LIBGL_ALWAYS_INDIRECT=1
      export MPICH_MPIIO_DVS_MAXNODES=32
      export DVS_MAXNODES=1__
      ulimit -Sc 0
      #ZZ 12/01/2015
      if [ -z "$CSHRCREAD" ]; then
        readonly CSHRCREAD=true
        export CSHRCREAD
      fi
      if [ -n "$MODULESHOME" ]; then
        module load altd
        module load darshan
	#export MODULEPATH=/usr/common/software/custom_default_modulefiles:${MODULEPATH}

      fi
      export CSCRATCH=/global/cscratch1/sd/$USER
      ;;
    edison)
      export CRAY_CPU_TARGET=sandybridge
      export MAN_POSIXLY_CORRECT=1
      export LIBGL_ALWAYS_INDIRECT=1
      export MPICH_MPIIO_DVS_MAXNODES=14
      export DVS_MAXNODES=1__
      export MPICH_GNI_ROUTING_MODE=ADAPTIVE_1
      #ZZ 1/15/2016 to disable the --craype-buildtools-check in craype/2.5.0 
      export CRAYPE_USE_BUILDTOOLS=0
      ulimit -Sc 0
      if [ -n "$MODULESHOME" ]; then
        module load altd
        module load darshan
	#ZZ removed on 7/25/2018 export MODULEPATH=/usr/common/software/custom_default_modulefiles:${MODULEPATH}
      fi
      export CSCRATCH=/global/cscratch1/sd/$USER
    ;;
    alva)
      export MPICH_MPIIO_DVS_MAXNODES=2
      export DVS_MAXNODES=1__
      ulimit -Sc 0
    ;;
    pdsf)
#   On PDSF default file group is not the personal file group; 
#   it is a "project" (experiment) group
#   but users have asked for group readable
#    umask 77
      NCHOS='none'
      if [ -x /usr/common/usg/bin/chosenv ]
      then
        NCHOS=$(/usr/common/usg/bin/chosenv)
        if [ "$NCHOS" == "sl53" ] || [ "$NCHOS" == "test" ]
        then
          if [ -r /usr/common/usg/skel/sl53_global/pdsf.profile ]
          then
	    . /usr/common/usg/skel/sl53_global/pdsf.profile
	    if [ ! -e ${HOME}/.no_default_modules ]
	    then
	      if [ -n "${MODULESHOME}" ]
	      then
	        module load usg-default-modules
	      fi
	    fi
          fi
        fi
      fi
      export PATH="/usr/common/usg/wrappers:"$PATH
      unset NCHOS
      export HISTSIZE=50         # previous commands to remember
      export PS1="`hostname` $ " # command prompt 
      export PS2="more > "       # command prompt 
    ;;
    *)
      ;;
  esac

# User-specific settings

# Source appropriate .ext file

  SHELL_PARSING=$0
  if [ "$SHELL_PARSING" == "-su" ]
  then
    SHELL_PARSING=`readlink /proc/$$/exe`
  fi

  case "$SHELL_PARSING" in
    -sh|sh|*/sh)
      if [ -e $HOME/.profile.ext ]
      then
        . $HOME/.profile.ext
      fi 
      ;;
    -ksh|ksh|*/ksh)
      export ENV=$HOME/.kshrc
      if [ -e $HOME/.profile.ext ]
      then
        . $HOME/.profile.ext
      fi 
      ;;
    -bash|bash|*/bash)
      export BASH_ENV=$HOME/.bashrc
      if [ -e $HOME/.bash_profile.ext ]
      then
        . $HOME/.bash_profile.ext
      else 
        if [ -e $HOME/.profile.ext ]
        then
          . $HOME/.profile.ext
        fi
      fi
      if [ "$NERSC_HOST" != "edison" -a "$NERSC_HOST" != "cori" ] 
      then
        . $BASH_ENV
      fi
      ;; 
  esac

  [ -e $HOME/.dbgdot ] && echo "exiting .profile/.bash_profile" >&2
fi
# end .profile
