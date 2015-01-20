Dia2Scala
=========
This is a code generator - a console application. It works with one file from diagram editor [Dia](http://dia-installer.de/) as an input and generates Scala source code files on its output. UML lacks a lot of Scala features - some has been added via stereotypes (all supported stereotypes are present in test diagrams `src/test/resources/diagrams`; if anything is missing here, you can surely find it over there).


Examples
--------
[Showcase page](https://github.com/mnn/Dia2Scala/blob/master/other/Example.md)


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


Notation
--------
Following page shows custom stereotypes on diagrams and equivalent source code.
[Notation page](https://github.com/mnn/Dia2Scala/blob/master/other/Notation.md)


Not implemented yet
-------------------
All planned features are implemented.


Known issues
------------
Currently none.


Not supported
-------------
*  template classes (generic user classes)
*  splitting to methods when overrinding `def` with `var` (*workaround:* split those methods manually in child)
*  graphically nested packages (*workaround:* label packages with thier full names)
*  default values


Donation
--------
If you use this project, please consider a donation of any amount (author is a not-wealthy student).

[![PayPal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=U6PGB7P24WWSU&lc=CZ&item_name=monnef%20%2d%20Dia2Scala&currency_code=CZK&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted)


License
-------
GNU General Public License 3 (GPL3)  
For more details read `LICENSE.txt`.
