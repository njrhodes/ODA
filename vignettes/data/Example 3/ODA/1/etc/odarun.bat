@echo off
echo. & echo Beginning ODA run.... & echo.
start /wait cmd /k "megaoda.exe oda.pgm & pause >nul & exit"
echo. & echo Run complete. Cleaning up.... & echo.
echo off
del megaoda.exe
mkdir etc
move oda.pgm etc
mkdir inputs
move data.csv inputs
move data.txt inputs
mkdir outputs
move model* outputs
copy odarun.bat etc
echo. & echo Press any key to complete run and close this window... & echo.
pause > nul
erase out.out
erase odarun.bat
