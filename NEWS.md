# NEWS

# jaggernaut 1.5.4

* `update` now runs chains in parallel (if workers available)

# jaggernaut 1.5.3

* Fixed error when running in parallel on windows (was failing to export
needed functions)

# jaggernaut 1.5.2

* Upgraded to `datalist` 0.2.

# Bug Reports 

For more fine-grained list of changes or to report a bug, consult 

* [The commit log](https://github.com/joethorley/jaggernaut/commits/master)
* [The issues log](https://github.com/joethorley/jaggernaut/issues)

# Versioning

Releases are numbered with the following semantic versioning format:

\<major\>.\<minor\>.\<patch\>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch

For more information on jaggernaut, please visit: 
https://github.com/joethorley/jaggernaut/wiki.
