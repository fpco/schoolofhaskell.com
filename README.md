# School of Haskell 2.0

The School of Haskell, or SoH for short, is a place to share any variety of
Haskell info in a way that enables interactive experimentation. Tutorials,
documentation, blogposts, and pastes of nifty snippets of code are all welcome.

## Building

> stack build

## Running

> FP_ENVIRONMENT_NAME=development stack exec soh-site

## Contributing

Contributions are welcome! Please let us know when undertaking any big changes,
so that we can discuss design / implementation / generally coordinate.  No need
to do this for typical tweaks / clear fixes / minor enhancements.

This code was recently split off the rest of the code for www.fpcomplete.com.
So, if you see some code that doesn't seem to have a purpose, then that's why.

## TODO

* Update dependencies and compiler (switch to lts 3.*)

* Files which could use refinement:

  - templates/blank-my-content.hamlet
  - login.hamlet (needs tagline)

* Have the main school page explain what SoH is?

* Fix page vs content vs tutorial vernacular - ugh!  Not sure what to do about it.

* How should we have an admins list?  Currently it will be empty when running in
  production mode.

* Bring back "related content"?  It used to be a manually managed list.

These requires input from SoH service:

* Need a list of supported environments in the tutorial editor.

* Figure out how to map old SoH ghc envs to new SoH envs?
