set -e
unset PATH
for p in $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done

cp -r $src $TMP/

cd $TMP

COMPILE="$duebuilder/compile -O2"
LINK=$duebuilder/link
ERASE=$duebuilder/erase
BOSSAC=$duebuilder/bossac

PORT=$port
USB=$usb


set -o xtrace

# to make ventilator.c and ventilator.h files
$spec/bin/generate-c

$COMPILE $src/median.c -o median.o
$COMPILE -I$src/encoder/ $src/encoder/Encoder.cpp -o Encoder.o
$COMPILE -I$src/lcd/ $src/lcd/LiquidCrystal.cpp -o LiquidCrystal.o
$COMPILE -I$src/lcd/ $src/display.cpp -o display.o
$COMPILE -I$src/encoder $src/input.cpp -o input.o
$COMPILE $src/display_modes.cpp -o display_modes.o
# This one was generated
$COMPILE ./ventilator.c -o ventilator.o
$COMPILE -I./ -I$src/encoder $src/main.cpp -o main.o

$LINK main.o display.o input.o display_modes.o ventilator.o median.o \
  Encoder.o LiquidCrystal.o

mkdir $out
cp build.bin $out/
cd $out
# CAn't upload automatically because nix build user does not have permissions
# Instead just drop a script.
echo "$ERASE -F /dev/$port" > upload
echo "$BOSSAC -e -w $out/build.bin --boot=1 -v -R --port $port -U $usb" >> upload
chmod 755 upload
