Dia2Scala
=========
This is a code generator - a console application. It works with one file from diagram editor [Dia](http://dia-installer.de/) as an input and generates Scala source code files on its output. UML lacks a lot of Scala features - some has been added via stereotypes (all supported stereotypes are present in test diagrams `src/test/resources/diagrams`; if anything is missing here, you can surely find it over there).


Compilation
-----------
[SBT](http://www.scala-sbt.org/)


Command line arguments
----------------------
```
Usage: dia2scala [options]

  -f <file> | --file <file>
        input dia file
  -u | --unpacked
        skips unpacking of an input dia file
  --help
        prints this usage text
  -q | --quiet
        suppresses all non critical output
  -v | --verbose
        prints extra debug information
  -vv | --veryverbose
        prints a lot of debug information
  -o <path> | --outputpath <path>
        output directory
  -d | --groupbydependency
        tries group related classes to one source file
```


Example diagrams
----------------
**TODO**: pictures of diagrams with either short text description or corresponding code


License
-------
GNU General Public License 3 (GPL3)  
For more details read `LICENSE.txt`.
