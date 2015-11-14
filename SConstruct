# Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
# See the LICENSE.txt file at the top-level directory of this distribution.

import glob

env = Environment(
    F90PATH = "./build",
    FORTRANMODDIRPREFIX="-J",
    FORTRANMODDIR="./build",
    F90="gfortran",
    LINK="gfortran",
    F90FLAGS="-O3 -ffast-math -g -Wall",
)

sources = glob.glob("./src/*.f90")

env.SharedLibrary("flib", sources)
#env.Library("flib", sources)
