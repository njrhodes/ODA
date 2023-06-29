@echo off
echo. & echo Beginning CTA run.... & echo.
start /wait cmd /k "cta.exe cta.pgm & pause >nul & exit"
echo. & echo Run complete. Cleaning up.... & echo.
echo off
del cta.exe
mkdir etc
move cta.pgm etc
mkdir inputs
move data.csv inputs
move data.txt inputs
mkdir outputs
move model* outputs
copy ctarun.bat etc
echo. & echo Press any key to complete run and close this window... & echo.
pause > nul
erase ctarun.bat
