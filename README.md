# Lambda-man
## How to use : Lambda Man 
To use this project,make sure you are working on a GNU/Linux distribution and have installed Opam 2. <br>
then `$ ./configure ` will install all the project dependancies on a Opam installation. <br>
If everything went well , you will have the following message : <br>

> Congratulations! Your project is ready for development.

Now, you can compile with `$ make` and it will create an executable file called "lambda". <br>
Finally,you can launch the game with : 

`./lambda server -s 0.01 -v -w tests/00015-hell-and-suffering.json './lambda man -n 1  -v' `<br>
as :
* the server is going to use the world "00015-hell-and-suffering.json" in the "tests/" repository where there are 20 others worlds to test. (4 others personalized worlds are available in the "myworld/" repository)
* The option " -s 0.1 " tells the server to wait 0.1 sec before any action but we can replace it by another value like 0.001 to go faster.
* The option "-v" refers to a basic graphic version with arrow and recangles but you can replace it with " -g " to get some images.
* Then the server will execute the command in simple quote , './lambda man -n 1 -v' where the option "-n 1" refers to the number of robot to use and " -v " refers to the same option as precedent point.


