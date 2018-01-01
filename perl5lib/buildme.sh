#!/usr/bin/env bash
#
# $Id$
#
# This script builds all binary Perl modules required by ShairTunes2.
# 
# Supported OSes:
#
# Linux (Perl 5.8.8, 5.10.0, 5.12.4, 5.14.1, 5.16.3)
#   i386/x86_64 Linux
#   ARM Linux
#   PowerPC Linux
#   Sparc Linux (ReadyNAS)
# Mac OSX
#   Under 10.5, builds Universal Binaries for i386/ppc Perl 5.8.8
#   Under 10.6, builds Universal Binaries for i386/x86_64 Perl 5.10.0
#   Under 10.7, builds for x86_64 Perl 5.12.3 (Lion does not support 32-bit CPUs)
#   Under 10.9, builds for x86_64 Perl 5.16
#   Under 10.10, builds for x86_64 Perl 5.18
# FreeBSD 7.2 (Perl 5.8.9)
# FreeBSD 8.X,9.X (Perl 5.12.4)
# Solaris 
#   builds are best done with custom compiled perl and gcc
#   using the following PATH=/opt/gcc-5.1.0/bin:/usr/gnu/bin:$PATH
#   plus a path to a version of yasm and nasm
#
#   Tested versions (to be extended)
#     OmniOSCE 151022 LTS (Perl 5.24.1)
#     OmniOSCE 151024     (Perl 5.24.3)
#
# Perl 5.12.4/5.14.1 note:
#   You should build 5.12.4 using perlbrew and the following command. GCC's stack protector must be disabled
#   so the binaries will not be dynamically linked to libssp.so which is not available on some distros.
#   NOTE: On 32-bit systems for 5.12 and higher, -D use64bitint should be used. Debian Wheezy (the next release) will
#   use Perl 5.12.4 with use64bitint enabled, and hopefully other major distros will follow suit.
#
#     perlbrew install perl-5.12.4 -D usethreads -D use64bitint -A ccflags=-fno-stack-protector -A ldflags=-fno-stack-protector
#
#   For 64-bit native systems, use:
#
#     perlbrew install perl-5.12.4 -D usethreads -A ccflags=-fno-stack-protector -A ldflags=-fno-stack-protector
#

# Require modules to pass tests
RUN_TESTS=1
USE_HINTS=1
CLEAN=1
FLAGS="-fPIC"

function usage {
    cat <<EOF
$0 [args] [target]
-h            this help
-c            do not run make clean
-i <lmsbase>  install modules in lmsbase directory
-p <perlbin > set custom perl binary
-t            do not run tests

target: make target - if not specified all will be built

EOF
}

while getopts hci:p:t opt; do
  case $opt in
  c)
      CLEAN=0
      ;;
  i)
      LIBBASEDIR=$OPTARG
      ;;
  p)
      CUSTOM_PERL=$OPTARG
      ;;
  t)
      RUN_TESTS=0
      ;;
  h)
      usage
      exit
      ;;
  *)
      echo "invalid argument"
      usage
      exit 1
      ;;
  esac
done

shift $((OPTIND - 1))

echo "RUN_TESTS:$RUN_TESTS CLEAN:$CLEAN USE_HINTS:$USE_HINTS target ${1-all}"

OS=`uname`
MACHINE=`uname -m`

# get system arch, stripping out extra -gnu on Linux
ARCHPERL=/usr/bin/perl
if [ "$OS" = "FreeBSD" ]; then
    ARCHPERL=/usr/local/bin/perl
fi
ARCH=`$ARCHPERL -MConfig -le 'print $Config{archname}' | sed 's/gnu-//' | sed 's/^i[3456]86-/i386-/' | sed 's/armv.*?-/arm-/' `

if [ "$OS" = "Linux" -o "$OS" = "Darwin" -o "$OS" = "FreeBSD" -o "$OS" = "SunOS" ]; then
    echo "Building for $OS / $ARCH"
else
    echo "Unsupported platform: $OS, please submit a patch or provide us with access to a development system."
    exit
fi

# Set default values prior to potential overwrite for FreeBSD platforms
GCC=gcc
GXX=g++
GPP=cpp
if [ "$OS" = "FreeBSD" ]; then
    BSD_MAJOR_VER=`uname -r | sed 's/\..*//g'`
    BSD_MINOR_VER=`uname -r | sed 's/.*\.//g'`
    if [ -f "/etc/make.conf" ]; then
        MAKE_CC=`grep CC /etc/make.conf | grep -v CCACHE | grep -v \# | sed 's#CC=##g'`
        MAKE_CXX=`grep CXX /etc/make.conf | grep -v CCACHE | grep -v \# | sed 's#CXX=##g'`
        MAKE_CPP=`grep CPP /etc/make.conf | grep -v CCACHE | grep -v \# | sed 's#CPP=##g'`
    fi
    if [[ ! -z "$MAKE_CC" ]]; then
        GCC="$MAKE_CC"
    elif [ $BSD_MAJOR_VER -ge 10 ]; then
        # FreeBSD started using clang as the default compiler starting with 10.
        GCC=cc
    else
        GCC=gcc
    fi
    if [[ ! -z "$MAKE_CXX" ]]; then
        GXX="$MAKE_CXX"
    elif [ $BSD_MAJOR_VER -ge 10 ]; then
        # FreeBSD started using clang++ as the default compiler starting with 10.
        GXX=c++
    else
        GXX=g++
    fi
    if [[ ! -z "$MAKE_CPP" ]]; then
        GPP="$MAKE_CPP"
    else
        GPP=cpp
    fi
fi

for i in $GCC cpp rsync make ; do
    which $i > /dev/null
    if [ $? -ne 0 ] ; then
        echo "$i not found - please install it"
        exit 1
    fi
done

echo "Looks like your compiler is $GCC"
$GCC --version

# This method works for FreeBSD, with "normal" installs of GCC and clang.
CC_TYPE=`$GCC --version | head -1`

# Determine compiler type and version
CC_IS_CLANG=false
CC_IS_GCC=false
# Heavy wizardry begins here
# This uses bash globbing for the If statement
if [[ "$CC_TYPE" =~ "clang" ]]; then
    CLANG_MAJOR=`echo "#include <iostream>" | "$GXX" -xc++ -dM -E - | grep '#define __clang_major' | sed 's/.*__\ //g'`
    CLANG_MINOR=`echo "#include <iostream>" | "$GXX" -xc++ -dM -E - | grep '#define __clang_minor' | sed 's/.*__\ //g'`
    CLANG_PATCH=`echo "#include <iostream>" | "$GXX" -xc++ -dM -E - | grep '#define __clang_patchlevel' | sed 's/.*__\ //g'`
    CC_VERSION=`echo "$CLANG_MAJOR"."$CLANG_MINOR"."$CLANG_PATCH" | sed "s#\ *)\ *##g" | sed -e 's/\.\([0-9][0-9]\)/\1/g' -e 's/\.\([0-9]\)/0\1/g' -e 's/^[0-9]\{3,4\}$/&00/'`
    CC_IS_CLANG=true
elif [[ "$CC_TYPE" =~ "gcc" || "$CC_TYPE" =~ "GCC" ]]; then
    CC_IS_GCC=true
    CC_VERSION=`$GCC -dumpversion | sed "s#\ *)\ *##g" | sed -e 's/\.\([0-9][0-9]\)/\1/g' -e 's/\.\([0-9]\)/0\1/g' -e 's/^[0-9]\{3,4\}$/&00/'`
else
    echo "********************************************** ERROR ***************************************"
    echo "*"
    echo "*    You're not using GCC or clang. Somebody's playing a prank on me."
    echo "*    Cowardly choosing to abandon build."
    echo "*"
    echo "********************************************************************************************"
    exit 1
fi

if [[ "$CC_IS_GCC" == true && "$CC_VERSION" -lt 40200 ]]; then
    echo "********************************************** ERROR ****************************************"
    echo "*"
    echo "*    It looks like you're using GCC earlier than 4.2,"
    echo "*    Cowardly choosing to abandon build."
    echo "*    This is because modern ICU requires -std=c99"
    echo "*"
    echo "********************************************************************************************"
    exit 1
fi

if [[ "$CC_IS_CLANG" == true && "$CC_VERSION" -lt 30000 ]]; then
    echo "********************************************** ERROR ****************************************"
    echo "*"
    echo "*    It looks like you're using clang earlier than 3.0,"
    echo "*    Cowardly choosing to abandon build."
    echo "*    This is because modern ICU requires -std=c99"
    echo "*"
    echo "********************************************************************************************"
    exit 1
fi

if [[ ! -z `echo "#include <iostream>" | "$GXX" -xc++ -dM -E - | grep LIBCPP_VERSION` ]]; then
    GCC_LIBCPP=true
elif [[ ! -z `echo "#include <iostream>" | "$GXX" -xc++ -dM -E - | grep __GLIBCXX__` ]]; then
    GCC_LIBCPP=false
else
    echo "********************************************** NOTICE **************************************"
    echo "*"
    echo "*    Doesn't seem you're using libc++ or lc++ as your c++ library."
    echo "*    I will assume you're using the GCC stack, and that DBD needs -lstdc++."
    echo "*"
    echo "********************************************************************************************"
    GCC_LIBCPP=false
fi

PERL_CC=`$ARCHPERL -V | grep cc=\' | sed "s#.*cc=\'##g" | sed "s#\'.*##g"`

if [[ "$PERL_CC" != "$GCC" ]]; then
    echo "********************************************** WARNING *************************************"
    echo "*                                                                                          *"
    echo "*    Perl was compiled with $PERL_CC,"
    echo "*    which is different than $GCC."
    echo "*    This may cause significant problems.                                                  *"
    echo "*                                                                                          *"
    echo "* Press CTRL^C to stop the build now...                                                    *"
    echo "********************************************************************************************"
    sleep 3
fi

# figure out OSX version and customize SDK options
OSX_VER=
OSX_FLAGS=
OSX_ARCH=
if [ "$OS" = "Darwin" ]; then
    OSX_VER=`/usr/sbin/system_profiler SPSoftwareDataType`
    REGEX=' OS X.* (10\.[5-9])'
    REGEX2=' OS X.* (10\.1[0-9])'

    if [[ $OSX_VER =~ $REGEX ]]; then
        OSX_VER=${BASH_REMATCH[1]}
    elif [[ $OSX_VER =~ $REGEX2 ]]; then
        OSX_VER=${BASH_REMATCH[1]}
    else
        echo "Unable to determine OSX version"
        exit 0
    fi
    
    if [ "$OSX_VER" = "10.5" ]; then
        # Leopard, build for i386/ppc with support back to 10.4
        OSX_ARCH="-arch i386 -arch ppc"
        OSX_FLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.4"
    elif [ "$OSX_VER" = "10.6" ]; then
        # Snow Leopard, build for x86_64/i386 with support back to 10.5
        OSX_ARCH="-arch x86_64 -arch i386"
        OSX_FLAGS="-isysroot /Developer/SDKs/MacOSX10.5.sdk -mmacosx-version-min=10.5"
    elif [ "$OSX_VER" = "10.7" ]; then
        # Lion, build for x86_64 with support back to 10.6
        OSX_ARCH="-arch x86_64"
        OSX_FLAGS="-isysroot /Developer/SDKs/MacOSX10.6.sdk -mmacosx-version-min=10.6"
    elif [ "$OSX_VER" = "10.9" ]; then
        # Mavericks, build for x86_64 with support back to 10.9
        OSX_ARCH="-arch x86_64"
        OSX_FLAGS="-mmacosx-version-min=10.9"
    elif [ "$OSX_VER" = "10.10" ]; then
        # Yosemite, build for x86_64 with support back to 10.10
        OSX_ARCH="-arch x86_64"
        OSX_FLAGS="-mmacosx-version-min=10.10"
    fi
fi

# Build dir
BUILD=$PWD/build
PERL_BASE=$BUILD/perl5x
PERL_ARCH=$BUILD/arch/perl5x

# Path to Perl 5.8.8
if [ -x "/usr/bin/perl5.8.8" ]; then
    PERL_58=/usr/bin/perl5.8.8
elif [ -x "/usr/local/bin/perl5.8.8" ]; then
    PERL_58=/usr/local/bin/perl5.8.8
elif [ -x "$HOME/perl5/perlbrew/perls/perl-5.8.9/bin/perl5.8.9" ]; then
    PERL_58=$HOME/perl5/perlbrew/perls/perl-5.8.9/bin/perl5.8.9
elif [ -x "/usr/local/bin/perl5.8.9" ]; then # FreeBSD 7.2
    PERL_58=/usr/local/bin/perl5.8.9
fi

if [ $PERL_58 ]; then
    PERL_BIN=$PERL_58
    PERL_MINOR_VER=8
fi

# Path to Perl 5.10.0
if [ -x "/usr/bin/perl5.10.0" ]; then
    PERL_510=/usr/bin/perl5.10.0
elif [ -x "/usr/local/bin/perl5.10.0" ]; then
    PERL_510=/usr/local/bin/perl5.10.0
elif [ -x "/usr/local/bin/perl5.10.1" ]; then # FreeBSD 8.2
    PERL_510=/usr/local/bin/perl5.10.1
fi

if [ $PERL_510 ]; then
    PERL_BIN=$PERL_510
    PERL_MINOR_VER=10
fi

# Path to Perl 5.12
if [ "$OSX_VER" = "10.9" ]; then
    echo "Ignoring Perl 5.12 - we want 5.16 on Mavericks"
elif [ -x "/usr/bin/perl5.12.4" ]; then
    PERL_512=/usr/bin/perl5.12.4
elif [ -x "/usr/local/bin/perl5.12.4" ]; then
    PERL_512=/usr/local/bin/perl5.12.4
elif [ -x "/usr/local/bin/perl5.12.4" ]; then # Also FreeBSD 8.2
    PERL_512=/usr/local/bin/perl5.12.4
elif [ -x "$HOME/perl5/perlbrew/perls/perl-5.12.4/bin/perl5.12.4" ]; then
    PERL_512=$HOME/perl5/perlbrew/perls/perl-5.12.4/bin/perl5.12.4
elif [ -x "/usr/bin/perl5.12" ]; then
    # OSX Lion uses this path
    PERL_512=/usr/bin/perl5.12
fi

if [ $PERL_512 ]; then
    PERL_BIN=$PERL_512
    PERL_MINOR_VER=12
fi

# Path to Perl 5.14.1
if [ -x "$HOME/perl5/perlbrew/perls/perl-5.14.1/bin/perl5.14.1" ]; then
    PERL_514=$HOME/perl5/perlbrew/perls/perl-5.14.1/bin/perl5.14.1
fi

if [ $PERL_514 ]; then
    PERL_BIN=$PERL_514
    PERL_MINOR_VER=14
fi

# Path to Perl 5.16
if [ "$OSX_VER" = "10.10" ]; then
    echo "Ignoring Perl 5.16 - we want 5.18 on Yosemite"
elif [ -x "/usr/bin/perl5.16" ]; then
    PERL_516=/usr/bin/perl5.16
elif [ -x "/usr/bin/perl5.16.3" ]; then
    PERL_516=/usr/bin/perl5.16.3
fi

if [ $PERL_516 ]; then
    PERL_BIN=$PERL_516
    PERL_MINOR_VER=16
fi

# Path to Perl 5.18
if [ -x "/usr/bin/perl5.18" ]; then
    PERL_518=/usr/bin/perl5.18
fi

# defined on the command line - no detection yet
if [ $PERL_518 ]; then
    PERL_BIN=$PERL_518
    PERL_MINOR_VER=18
fi

# defined on the command line - no detection yet
if [ $PERL_520 ]; then
    PERL_BIN=$PERL_520
    PERL_MINOR_VER=20
fi

# Path to Perl 5.22
if [ -x "/usr/bin/perl5.22.1" ]; then
    PERL_522=/usr/bin/perl5.22.1
fi

if [ $PERL_522 ]; then
    PERL_BIN=$PERL_522
    PERL_MINOR_VER=22
fi

# Path to Perl 5.24
if [ -x "/usr/bin/perl5.24.1" ]; then
    PERL_524=/usr/bin/perl5.24.1
fi

if [ $PERL_524 ]; then
    PERL_BIN=$PERL_524
    PERL_MINOR_VER=24
fi

# Path to Perl 5.26
if [ -x "/usr/bin/perl5.26.0" ]; then
    PERL_526=/usr/bin/perl5.26.0
fi

if [ $PERL_526 ]; then
    PERL_BIN=$PERL_526
    PERL_MINOR_VER=26
fi

# try to use default perl version
if [ "$PERL_BIN" = "" -o "$CUSTOM_PERL" != "" ]; then
    if [ "$CUSTOM_PERL" = "" ]; then
        PERL_BIN=`which perl`
        PERL_VERSION=`perl -MConfig -le '$Config{version} =~ /(\d+.\d+)\./; print $1'`
    else
        PERL_BIN=$CUSTOM_PERL
        PERL_VERSION=`$CUSTOM_PERL -MConfig -le '$Config{version} =~ /(\d+.\d+)\./; print $1'`
    fi
    if [[ "$PERL_VERSION" =~ "5." ]]; then
        PERL_MINOR_VER=`echo "$PERL_VERSION" | sed 's/.*\.//g'`
    else
        echo "Failed to find supported Perl version for '$PERL_BIN'"
        exit
    fi

fi

echo "Building with Perl 5.$PERL_MINOR_VER at $PERL_BIN"
PERL_BASE=$BUILD/5.$PERL_MINOR_VER
PERL_ARCH=$BUILD/arch/5.$PERL_MINOR_VER


# FreeBSD's make sucks
if [ "$OS" = "FreeBSD" ]; then
    if [ ! -x /usr/local/bin/gmake ]; then
        echo "ERROR: Please install GNU make (gmake)"
        exit
    fi
    export MAKE=/usr/local/bin/gmake
elif [ "$OS" = "SunOS" ]; then
    if [ ! -x /usr/bin/gmake ]; then
        echo "ERROR: Please install GNU make (gmake)"
        exit
    fi 
    export MAKE=/usr/bin/gmake
else
    # Support a newer make if available, needed on ReadyNAS                                                                              
    if [ -x /usr/local/bin/make ]; then                                               
        export MAKE=/usr/local/bin/make                                         
    else                                                                           
        export MAKE=/usr/bin/make                        
    fi
fi

#  Clean up
if [ $CLEAN -eq 1 ]; then
    rm -rf $BUILD/arch
fi

mkdir -p $PERL_ARCH

# $1 = args
# $2 = file
function tar_wrapper {
    echo "tar $1 $2"
    tar $1 "$2" > /dev/null
    echo "tar done"
}


# $1 = module to build
# $2 = Makefile.PL arg(s)
# $3 = run tests if 1 - default to $RUN_TESTS
# $4 = make clean if 1 - default to $CLEAN
# $5 = use hints if 1 - default to $USE_HINTS
function build_module {
    module=$1
    makefile_args=$2
    local_run_tests=${3-$RUN_TESTS}
    local_clean=${4-$CLEAN}
    local_use_hints=${5-$USE_HINTS}

    echo "build_module run tests:$local_run_tests clean:$local_clean hints $local_use_hints $module $makefile_args"

    if [ ! -d $module ]; then

        if [ ! -f "${module}.tar.gz" ]; then
            echo "ERROR: cannot find source code archive ${module}.tar.gz"
            echo "Please download all source files from http://github.com/Logitech/slimserver-vendor"
            exit
        fi

        tar_wrapper zxvf "${module}.tar.gz"
    fi

    cd "${module}"
    
    if [ $local_use_hints -eq 1 ]; then
        # Always copy in our custom hints for OSX
        cp -Rv ../hints .
    fi
    if [ $PERL_BIN ]; then
        export PERL5LIB=$PERL_BASE/lib/perl5
        
        $PERL_BIN Makefile.PL INSTALL_BASE=$PERL_BASE $makefile_args
        if [ $local_run_tests -eq 1 ]; then
            $MAKE test
        else
            $MAKE
        fi
        if [ $? != 0 ]; then
            if [ $local_run_tests -eq 1 ]; then
                echo "make test failed, aborting"
            else
                echo "make failed, aborting"
            fi
            exit $?
        fi
        $MAKE install

        if [ $local_clean -eq 1 ]; then
            $MAKE clean
        fi
    fi

    cd ..
    rm -rf $module
}

function build_all {
     build Net::SDP
     build Crypt::OpenSSL::Bignum
     build Crypt::OpenSSL::RSA
}

function build {
    case "$1" in
        Net::SDP)
            build_module inc-latest-0.500
            build_module Module-Build-0.4224
            build_module Net-SDP-0.07
            ;;

        Crypt::OpenSSL::Bignum)
            build_module Crypt-OpenSSL-Bignum-0.06
            ;;

        Crypt::OpenSSL::RSA)
            build_module Crypt-OpenSSL-Random-0.11
            build_module Crypt-OpenSSL-RSA-0.28
    esac
}

# Build a single module if requested, or all
if [ $1 ]; then
    echo "building only $1"
    build $1
else
    build_all
fi

# Reset PERL5LIB
export PERL5LIB=

if [ "$OS" = 'Darwin' ]; then
    # strip -S on all bundle files
    find $BUILD -name '*.bundle' -exec chmod u+w {} \;
    find $BUILD -name '*.bundle' -exec strip -S {} \;
elif [ "$OS" = 'Linux' -o "$OS" = "FreeBSD" ]; then
    # strip all so files
    find $BUILD -name '*.so' -exec chmod u+w {} \;
    find $BUILD -name '*.so' -exec strip {} \;
fi

# clean out useless .bs/.packlist files, etc
find $BUILD -name '*.bs' -exec rm -f {} \;
find $BUILD -name '*.packlist' -exec rm -f {} \;

# create our directory structure
# rsync is used to avoid copying non-binary modules or other extra stuff
if [ $PERL_MINOR_VER -ge 12 ]; then
    # Check for Perl using use64bitint and add -64int
    ARCH=`$PERL_BIN -MConfig -le 'print $Config{archname}' | sed 's/gnu-//' | sed 's/^i[3456]86-/i386-/' | sed 's/armv.*?-/arm-/' `
fi
mkdir -p $PERL_ARCH/$ARCH
rsync -amv --include='*/' --include='*.pm' --exclude='*' $PERL_BASE/lib/perl5/$ARCH/ $PERL_ARCH/
rsync -amv --exclude=$ARCH --exclude='inc/' --exclude='Module/' --include='*/' --include='*.pm' --exclude='*' $PERL_BASE/lib/perl5/ $PERL_ARCH/
rsync -amv --include='*/' --include='*.so' --include='*.al' --include='*.ix' --exclude='*' $PERL_BASE/lib/perl5/$ARCH/auto $PERL_ARCH/$ARCH/

if [ $LIBBASEDIR ]; then
    if [ ! -d $LIBBASEDIR/5.$PERL_MINOR_VER/$ARCH ]; then
        mkdir -p $LIBBASEDIR/5.$PERL_MINOR_VER/$ARCH
    fi
    rsync -amv --exclude=$ARCH --include='*/' --include='*' $PERL_ARCH/ $LIBBASEDIR/
    rsync -amv --include='*/' --include='*.so' $PERL_ARCH/$ARCH/ $LIBBASEDIR/5.$PERL_MINOR_VER/$ARCH/
fi

# could remove rest of build data, but let's leave it around in case
#rm -rf $PERL_BASE
#rm -rf $PERL_ARCH
#rm -rf $BUILD/bin $BUILD/etc $BUILD/include $BUILD/lib $BUILD/man $BUILD/share $BUILD/var
