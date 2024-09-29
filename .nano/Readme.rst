***************************************
Improved Nano Syntax Highlighting Files
***************************************

This repository holds ``{lang}.nanorc`` files that have improved
definitions of syntax highlighting for various languages.
These should be placed inside of the ``~/.nano/`` directory.
Alternatively::

    git clone git@github.com:scopatz/nanorc.git ~/.nano
    
*Note - if you have any issues, alternatively use::

    git clone https://github.com/scopatz/nanorc.git ~/.nano


Once there you should add the languages you want to your
nano configuration file ``~/.nanorc``.  For example::

    ## C/C++
    include "~/.nano/c.nanorc"

You can also append the contents of ``~/.nano/nanorc`` into your
``~/.nanorc`` to include all languages::

    cat ~/.nano/nanorc >> ~/.nanorc
    
Finally, you can run an automatic installer using the following code::

    $ curl https://raw.githubusercontent.com/scopatz/nanorc/master/install.sh | sh

# trailing whitespace
color ,blue "[[:space:]]+$"
# multiples of eight spaces at the start a line
# (after zero or more tabs) should be a tab
color ,blue "^([TAB]*[ ]{8})+"
# tabs after spaces
color ,yellow "( )+TAB"
# highlight indents that have an odd number of spaces
color ,red "^(([ ]{2})+|(TAB+))*[ ]{1}[^ ]{1}"
# lines longer than 100 characters
color ,yellow "^(.{100})|(TAB.{63})|(TAB{2}.{55})|(TAB{3}.{47}).+$"
