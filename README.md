PHP Formatter
=============

This PHP script will format another PHP script using my arbitrarily set formatting rules (real tabs, 97 chars per line, brackets on their own line, indentation based on operator hierarchy not alignment, spaces around most operators but not commas or brackets, space after if, etc.). It will also format inline HTML, and if JS Beautify is available, it'll use that for inline Javascript in inline HTML too.

For an example of its usage, you can see the format.php itself, which was self-formatted by running:

    php -q format.php format.php format.php

Bugs
----
There are a couple known bugs, but nothing show-stopping right now. The most notable of these is that single-line comment blocks sometimes get confused about their line length, and sometimes when the operator precedence is re-biasing a space ends up on the beginning or end of the line. It shouldn't clobber anything, though, which is the most important thing. In general more testing is definitely necessary here.

Misc
----
This code is licensed under the MIT license included. See http://nathan.ca/code/license for more details.

For more information about this formatter, see http://nathan.ca/2013/01/php-formatter

Last updated: January 1, 2013
