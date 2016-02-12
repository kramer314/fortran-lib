# Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
# See the LICENSE.txt file at the top-level directory of this distribution.

import os

source_dir = "#src/"
build_dir = "#build/"

env = DefaultEnvironment(ENV = os.environ, TOOLS = ['default', "gfortran"])

# See https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html
debug_flags = "-Og -g3 -Wall -Wextra -Wconversion -Wunused-parameter -pedantic -fcheck=all -fbacktrace -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans"

env.Replace(F90FLAGS = debug_flags)
env.Replace(FORTRANMODDIRPREFIX = "-J ")
env.Replace(FORTRANMODDIR = build_dir)

Export("env")

SConscript(source_dir+"SConscript", variant_dir=build_dir, duplicate=1)

# For whatever reason, we can't use duplicate=0 and have *.mod files in the
# build directory. But, if we duplicate the source tree into the build
# directory SCons doesn't automatically clean the source files, so we have to
# manually define the entire build directory as a cleaning target.
Clean(".", build_dir)
