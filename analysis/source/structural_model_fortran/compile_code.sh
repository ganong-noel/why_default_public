#! /bin/sh

# eg. ./compile_code.sh armi config_nml.nml
out_file=$1

if [ -z "$2" ]
then
  config=config_nml.nml
else
  config="$2"
fi

cp $config ./src/
cd ./src

gfortran -c ntoil.f90 -Wall
gfortran -c Spline.f90 -Wall
gfortran -c Splint.f90 -Wall
gfortran -c Gettrans.f90 -Wall
if [ "${fortran_dir}" != "Rent" ]
then
  gfortran -c irevi.f90 -Wall
fi
gfortran -c irutil2.f90 -Wall
gfortran -c irevrenti.f90 -Wall
gfortran -o $out_file $out_file.f90 *.o -Wall
rm *.o
cd ..
