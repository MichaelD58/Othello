# CS2006 H2 - Othello

### Compiling this program
Compilation of this program can be done through two methods - cabal or ghc

Before compilation with either method, please ensure cabal's package list is up to date by running 'cabal update', and please ensure that the System.Random module is installed by running 'cabal install random'.

To compile this program with ghc, navigate to the 'src' directory and run 'ghc Main.hs'. This will create the executable file 'Main'.

To compile this program with cabal, remain outside of the 'src' directory and execute 'cabal build'. This will create an executable called 'Othello' in the 'bin' subdirectory within the 'dist' directory that cabal will create.

### Running this program
The program can be run from the relevant executable. This executable can be run without command line arguments to use the default game settings, or custom settings can be supplied as follows:

./\<executable\> [\<player-colour\> \<board-size\> <custom-start/normal-start> <hints-on/hints-off> <easy/hard>]
  
If a game has been saved before quitting, the last save game can be reloaded by running the program as follows:

./\<executable\> reload
